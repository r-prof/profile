# Plan: Profile Data Structure v2

## Motivation

The current profile data structure (v1) was designed before `dm` existed and has several limitations:

1. **Pre-aggregation**: Identical stack traces are collapsed with a `value` count, losing individual sample identity. This prevents attaching per-sample metadata (timestamps, memory snapshots).
2. **Nested list columns**: `samples$locations` stores a list of tibbles, which complicates querying and is not representable in a relational model without unnesting.
3. **No memory profiling support**: The format has no type-stable way to store optional memory profiling data alongside time samples.
4. **No provenance**: There is no way to identify the source of profiling data, making it impossible to combine profiles from multiple runs while tracking origin.
5. **Container**: The primary container is a named list (`profile_data`), rather than a `dm` object with built-in constraint checking.

## Requirements

1. Store raw data faithfully, without pre-aggregation
2. Allow optional memory profiling alongside time profiling, in a type-stable way (long form)
3. Use `dm` as the primary container
4. Store the source/provenance of profiling data
5. Versioned format with conversion from v1 (legacy)

## Current Data Model (v1)

```
profile_data (named list of tibbles)
├── meta:         key (chr), value (chr)
├── sample_types: type (chr), unit (chr)
├── samples:      value (int), locations (list of tibbles with location_id)
├── locations:    location_id (int), function_id (int), line (int)
└── functions:    function_id (int), name (chr), system_name (chr), filename (chr), start_line (int)
```

Key issues:
- `samples$locations` is a nested list column — not relational
- `samples$value` aggregates identical traces — loses individual samples
- No provenance tracking
- `dm_from_profile()` already unnests into a bridge table (`samples_locations`) — this should be the canonical form

## Proposed Data Model (v2)

The new data structure uses a `dm` object as the primary container with six tables:

```
dm object
├── meta:              key (chr), value (chr)
├── sources:           source_id (int), source_type (chr), source_uri (chr), source_timestamp (dbl)
├── samples:           sample_id (int), source_id (int)
├── sample_values:     sample_id (int), type (chr), unit (chr), value (dbl)
├── sample_locations:  sample_id (int), depth (int), location_id (int)
├── locations:         location_id (int), function_id (int), line (int)
└── functions:         function_id (int), name (chr), system_name (chr), filename (chr), start_line (int)
```

### Table descriptions

#### `meta`

Unchanged from v1, except the version value becomes `"2.0"`.

| Column | Type | Description |
|--------|------|-------------|
| `key`  | chr  | Metadata key |
| `value`| chr  | Metadata value |

Required row: `key = "version"`, `value = "2.0"`.

#### `sources`

New table. Tracks the provenance of profiling data so that multiple profiles can be combined.

| Column | Type | Description |
|--------|------|-------------|
| `source_id` | int | Primary key |
| `source_type` | chr | Origin type: `"rprof"`, `"pprof"`, `"manual"`, etc. |
| `source_uri` | chr | File path, URL, or identifier of the source |
| `source_timestamp` | dbl | Epoch timestamp (seconds since 1970-01-01 UTC) when the profile was captured, `NA` if unknown |

Primary key: `source_id`.

#### `samples`

One row per raw sample (no aggregation). Each sample is linked to a source.

| Column | Type | Description |
|--------|------|-------------|
| `sample_id` | int | Primary key |
| `source_id` | int | Foreign key → `sources` |

Primary key: `sample_id`.
Foreign key: `source_id` → `sources.source_id`.

#### `sample_values`

Long-form table for sample measurements. This is the type-stable way to store both time and memory profiling data.

| Column | Type | Description |
|--------|------|-------------|
| `sample_id` | int | Foreign key → `samples` |
| `type` | chr | Measurement type: `"samples"`, `"alloc_size"`, `"dealloc_size"`, etc. |
| `unit` | chr | Measurement unit: `"count"`, `"bytes"`, etc. |
| `value` | dbl | The measured value |

Foreign key: `sample_id` → `samples.sample_id`.
Compound key: (`sample_id`, `type`).

This replaces the separate `sample_types` table and the `samples$value` column from v1. Each sample can have multiple measurements (e.g., one row for the time sample count and one row for allocated bytes), enabling optional memory profiling in a type-stable long form.

#### `sample_locations`

Bridge table linking samples to their stack trace locations. Replaces the nested list column `samples$locations` from v1.

| Column | Type | Description |
|--------|------|-------------|
| `sample_id` | int | Foreign key → `samples` |
| `depth` | int | Position in the stack trace (1 = innermost) |
| `location_id` | int | Foreign key → `locations` |

Foreign key: `sample_id` → `samples.sample_id`.
Foreign key: `location_id` → `locations.location_id`.
Compound key: (`sample_id`, `depth`).

#### `locations`

Unchanged from v1.

| Column | Type | Description |
|--------|------|-------------|
| `location_id` | int | Primary key |
| `function_id` | int | Foreign key → `functions`, `NA` allowed |
| `line` | int | Source line number, 0 if unknown, `NA` allowed |

#### `functions`

Unchanged from v1.

| Column | Type | Description |
|--------|------|-------------|
| `function_id` | int | Primary key |
| `name` | chr | Demangled function name |
| `system_name` | chr | Mangled/raw function name |
| `filename` | chr | Source file name |
| `start_line` | int | Start line in source file, 0 if unknown |

### Relational diagram

```
sources 1──* samples 1──* sample_values
                     1──* sample_locations *──1 locations *──1 functions
```

### Key design decisions

1. **No pre-aggregation**: Each sample gets its own row in `samples`. Aggregation (run-length encoding of identical traces) is a downstream concern for analysis, not storage.

2. **Long-form sample values**: The `sample_values` table stores measurements in long form. A time-only profile has one row per sample; a profile with memory data has multiple rows per sample. This is type-stable (always the same columns) and extensible (new measurement types require no schema changes).

3. **Bridge table for locations**: The `sample_locations` table with a `depth` column replaces the nested list column approach. This is fully relational and works naturally with `dm`.

4. **Provenance via `sources`**: Each sample is linked to a `source`, enabling combination of multiple profiles. The `source_type` and `source_uri` columns provide traceability.

5. **`dm` as primary container**: The `dm` object is the canonical format, with primary and foreign keys defined. The `profile_data` class wraps or is replaced by `dm`.

## Migration: v1 → v2

### Conversion function

`profile_v2_from_v1(x)` converts a v1 `profile_data` object to a v2 `dm` object:

1. Create a single `sources` row with `source_type` inferred from the hidden `.rprof` or `.msg` component, `source_uri = NA` (not stored in v1).
2. Expand `samples`: unnest the `value` column by repeating each sample `value` times, assigning unique `sample_id`s.
3. Create `sample_values`: one row per sample with `type = "samples"`, `unit = "count"`, `value = 1`.
4. Create `sample_locations`: unnest `samples$locations`, adding `depth` based on row position within each sample.
5. Copy `locations` and `functions` unchanged.
6. Set `meta$value` to `"2.0"`.

### Version detection

`validate_profile()` inspects `meta$version` to determine which validation rules to apply. The v1 validation remains for backward compatibility.

### Backward compatibility

- `read_rprof()` and `read_pprof()` return v2 format by default, with an option to return v1 for backward compatibility.
- `write_rprof()` and `write_pprof()` accept both v1 and v2 formats.
- `dm_from_profile()` is retained for v1 objects; v2 objects are already `dm` objects.

## Implementation plan

### Phase 1: Foundation

- [ ] Add `dm` to `Imports` (currently in `Suggests`)
- [ ] Define `new_profile_v2()` constructor that creates a `dm` with all keys
- [ ] Define `validate_profile_v2()` validation for the v2 format
- [ ] Implement `profile_v2_from_v1()` conversion function

### Phase 2: Reader updates

- [ ] Update `rprof_to_ds()` to produce v2 format directly
- [ ] Update `msg_to_ds()` to produce v2 format directly
- [ ] Add `source_uri` parameter to `read_rprof()` and `read_pprof()`
- [ ] Store provenance in the `sources` table

### Phase 3: Writer updates

- [ ] Update `write_rprof()` to accept v2 format
- [ ] Update `write_pprof()` to accept v2 format
- [ ] Handle aggregation (collapsing identical traces) in the writer layer

### Phase 4: Memory profiling support

- [ ] Support reading Rprof memory profiling data into `sample_values`
- [ ] Support writing memory profiling data to pprof format

### Phase 5: Profile combination

- [ ] Implement `combine_profiles()` to merge multiple v2 profiles
- [ ] Ensure `source_id` and other IDs are remapped to avoid conflicts
- [ ] Preserve provenance across combinations

## Open questions

1. Should `dm` move from `Suggests` to `Imports`? This adds a dependency but makes the v2 format first-class. Alternative: keep `dm` in `Suggests` and use a plain list internally, constructing `dm` on demand.
2. Should the `profile_data` class be retained as a wrapper around `dm`, or should v2 objects simply be `dm` objects with a subclass?
3. Should `sample_values` use `dbl` for `value` to accommodate both counts and byte sizes, or should it be polymorphic (e.g., a `vctrs` type)?
4. Should `depth` in `sample_locations` be 1-indexed (R convention) or 0-indexed (pprof convention)?
