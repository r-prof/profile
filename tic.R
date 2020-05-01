do_package_checks()

# Build only for master or release branches
if (ci_has_env("BUILD_PKGDOWN") && grepl("^master$|^r-|^docs$", ci_get_branch())) {
  do_pkgdown(deploy = ci_can_push())
}
