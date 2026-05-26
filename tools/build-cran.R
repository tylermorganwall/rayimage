# Build a CRAN-style source package without modifying the working tree.
# This helper copies the package to a temporary directory, removes Remotes from
# DESCRIPTION in that copy only, and runs R CMD build on the copy. Extra command
# line arguments are passed through to R CMD build. Use --output-dir=PATH to
# choose where the resulting tarball is written.

args = commandArgs(trailingOnly = TRUE)

script_arg = grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_arg)) {
  script_path = normalizePath(
    sub("^--file=", "", script_arg[[1]]),
    mustWork = TRUE
  )
} else {
  script_path = normalizePath("tools/build-cran.R", mustWork = TRUE)
}

pkg_root = normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)
desc_path = file.path(pkg_root, "DESCRIPTION")
desc = read.dcf(desc_path)
pkg_name = desc[[1, "Package"]]

output_arg = grep("^--output-dir=", args, value = TRUE)
if (length(output_arg) > 1L) {
  stop("Use at most one --output-dir argument.", call. = FALSE)
}

if (length(output_arg) == 1L) {
  output_dir = sub("^--output-dir=", "", output_arg)
  build_args = args[args != output_arg]
} else {
  tmp_base = Sys.getenv("TMPDIR", unset = "")
  if (!nzchar(tmp_base)) {
    tmp_base = dirname(tempdir())
  }
  output_dir = file.path(
    tmp_base,
    sprintf(
      "%s-cran-build-%s-%s",
      pkg_name,
      format(Sys.time(), "%Y%m%d-%H%M%S"),
      Sys.getpid()
    )
  )
  build_args = args
}

output_dir = normalizePath(output_dir, mustWork = FALSE)
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
}
if (!dir.exists(output_dir)) {
  stop("Could not create output directory: ", output_dir, call. = FALSE)
}

copy_parent = tempfile(pattern = paste0(pkg_name, "-cran-src-"))
dir.create(copy_parent)
on.exit(unlink(copy_parent, recursive = TRUE, force = TRUE), add = TRUE)

tmp_pkg = file.path(copy_parent, pkg_name)
dir.create(tmp_pkg)

entries = list.files(pkg_root, all.files = TRUE, no.. = TRUE, full.names = TRUE)
entries = entries[basename(entries) != ".git"]
ok = file.copy(entries, tmp_pkg, recursive = TRUE, copy.date = TRUE)
if (!all(ok)) {
  failed = paste(basename(entries)[!ok], collapse = ", ")
  stop("Could not copy package files: ", failed, call. = FALSE)
}

tmp_desc_path = file.path(tmp_pkg, "DESCRIPTION")
tmp_desc = read.dcf(tmp_desc_path, all = TRUE)
if ("Remotes" %in% colnames(tmp_desc)) {
  tmp_desc = tmp_desc[, setdiff(colnames(tmp_desc), "Remotes"), drop = FALSE]
  write.dcf(tmp_desc, file = tmp_desc_path)
}

r_bin = file.path(R.home("bin"), "R")
cmd_args = c("CMD", "build", build_args, tmp_pkg)
old_wd = setwd(output_dir)
on.exit(setwd(old_wd), add = TRUE)
build_output = system2(
  r_bin,
  cmd_args,
  stdout = TRUE,
  stderr = TRUE,
  env = character()
)
cat(paste(build_output, collapse = "\n"), "\n", sep = "")

status = attr(build_output, "status")
if (is.null(status)) {
  status = 0L
}
if (!identical(as.integer(status), 0L)) {
  quit(status = as.integer(status), save = "no")
}

tarballs = list.files(output_dir, pattern = "\\.tar\\.gz$", full.names = TRUE)
if (!length(tarballs)) {
  stop(
    "R CMD build completed but no source tarball was found in ",
    output_dir,
    call. = FALSE
  )
}

info = file.info(tarballs)
tarball = tarballs[order(info$mtime, decreasing = TRUE)][[1]]
cat(normalizePath(tarball, mustWork = TRUE), "\n", sep = "")
