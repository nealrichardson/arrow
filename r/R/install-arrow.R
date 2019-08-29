# Licensed to the Apache Software Foundation (ASF) under one
# or more contributor license agreements.  See the NOTICE file
# distributed with this work for additional information
# regarding copyright ownership.  The ASF licenses this file
# to you under the Apache License, Version 2.0 (the
# "License"); you may not use this file except in compliance
# with the License.  You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied.  See the License for the
# specific language governing permissions and limitations
# under the License.

#' Help installing the Arrow C++ library
#'
#' Binary package installations should come with a working Arrow C++ library,
#' but when installing from source, you'll need to obtain the C++ library
#' first. This function offers guidance on how to get the C++ library depending
#' on your operating system and package version.
#' @export
#' @importFrom utils packageVersion
#' @examples
#' install_arrow()
install_arrow <- function() {
  os <- tolower(Sys.info()[["sysname"]])
  # c("windows", "darwin", "linux", "sunos") # win/mac/linux/solaris
  version <- packageVersion("arrow")
  message(install_arrow_msg(arrow_available(), version, os))
}

install_arrow_msg <- function(has_arrow, version, os) {
  # TODO: check if there is a newer version on CRAN?

  # Based on these parameters, assemble a string with installation advice
  if (has_arrow) {
    # Respond that you already have it
    msg <- ALREADY_HAVE
  } else if (os == "sunos") {
    # Good luck with that.
    msg <- c(SEE_DEV_GUIDE, THEN_REINSTALL)
  } else if (os == "linux") {
    # install_arrow() sends "version" as a "package_version" class, but for
    # convenience, this also accepts a string like "0.13.0". Calling
    # `package_version` is idempotent so do it again, and then `unclass` to get
    # the integers. Then see how many there are.
    dev_version <- length(unclass(package_version(version))[[1]]) > 3
    if (dev_version) {
      # Point to compilation instructions on readme
      msg <- c(SEE_DEV_GUIDE, THEN_REINSTALL)
    } else {
      # Release version. See if we can find installation instructions tailored
      # to this distro and version
      msg <- tryCatch(
        install_arrow_linux(
          tolower(system2("lsb_release", "-is", stdout = TRUE, stderr = TRUE)),
          system2("lsb_release", "-cs", stdout = TRUE, stderr = TRUE)
        ),
        error = function (e) {
          # Suggest arrow.apache.org/install, or compilation instructions
          c(paste(SEE_ARROW_INSTALL, OR_SEE_DEV_GUIDE), THEN_REINSTALL)
        }
      )
    }
  } else {
    # We no longer allow builds without libarrow on macOS or Windows so this
    # case shouldn't happen
    msg <- NULL
  }
  if (!is.null(msg)) {
    # NULL message means arrow has been installed successfully
    # Otherwise add common postscript
    msg <- c(msg, SEE_README, REPORT_ISSUE)
    msg <- paste(msg, collapse="\n\n")
  }
  msg
}

ALREADY_HAVE <- paste(
  "It appears you already have Arrow installed successfully:",
  "are you trying to install a different version of the library?"
)

SEE_DEV_GUIDE <- paste(
  "See the Arrow C++ developer guide",
  "<https://arrow.apache.org/docs/developers/cpp.html>",
  "for instructions on building the library from source."
)
# Variation of that
OR_SEE_DEV_GUIDE <- paste0(
  "Or, s",
  substr(SEE_DEV_GUIDE, 2, nchar(SEE_DEV_GUIDE))
)

SEE_ARROW_INSTALL <- paste(
  "See the Apache Arrow project installation page",
  "<https://arrow.apache.org/install/>",
  "to find pre-compiled binary packages for some common Linux distributions,",
  "including Debian, Ubuntu, and CentOS. You'll need to install",
  "'libparquet-dev' on Debian and Ubuntu, or 'parquet-devel' on CentOS. This",
  "will also automatically install the Arrow C++ library as a dependency."
)

THEN_REINSTALL <- paste(
  "After you've installed the C++ library,",
  "you'll need to reinstall the R package from source to find it."
)

SEE_README <- paste(
  "Refer to the R package README",
  "<https://github.com/apache/arrow/blob/master/r/README.md>",
  "for further details."
)

REPORT_ISSUE <- paste(
  "If you have other trouble, or if you think this message could be improved,",
  "please report an issue here:",
  "<https://issues.apache.org/jira/projects/ARROW/issues>"
)

# See https://arrow.apache.org/install/
#' @importFrom utils install.packages
install_arrow_linux <- function(distro, version) {
  # If any of this errors (including calling lsb_release to get distro/version)
  # the calling function will fall back to referring the user to the readme/docs

  # In case system2 returned an error code instead of raising an R error:
  if (!is.character(distro) || !is.character(version)) {
    stop("Could not identify distro/version", call.=FALSE)
  }
  if (distro == "centos") {
    commands <- CENTOS_INSTALL
  } else if (distro == "ubuntu") {
    commands <- sprintf(UBUNTU_INSTALL, version)
  } else if (distro == "debian") {
    commands <- sprintf(DEBIAN_INSTALL, distro, distro, version, distro, version)
    if (version == "stretch") {
      commands <- paste(
        sprintf(DEBIAN_BACKPORT, version),
        commands,
        sep = "\n"
      )
    }
  } else {
    stop("We don't have helpful commands for you", call. = FALSE)
  }

  instructions <- paste(
    "Try running this at the command line (as root):\n",
    commands,
    'R -e \'install.packages("arrow", repos = "https://cloud.r-project.org")\'',
    sep = "\n"
  )
  if (is_interactive()) {
    message(instructions)
    do_it <- readline("Would you like to run this now? (Y/n) ")
    if (do_it %in% c("", "Y", "y")) {
      script_file <- tempfile()
      cat(commands, file = script_file)
      status <- system(paste("sudo", "source", shQuote(script_file)))
      if (status > 0) {
        stop("Failed to install Arrow C++ libraries", call. = FALSE)
      }
      install.packages("arrow")
      # TODO (npr): detach and reload
      # ? Add devtools to Suggests and devtools::reload?

      # If successful (i.e. no errors), no further message needed
      instructions <- NULL
    } else {
      # We already printed the instructions.
      # Return an empty string so that the "See also README" gets printed.
      instructions <- ""
    }
  }
  instructions
}

# Copy here for mocking purposes
is_interactive <- function () interactive()

UBUNTU_INSTALL <- paste(
  'curl https://dist.apache.org/repos/dist/dev/arrow/KEYS | apt-key add -',
  'add-apt-repository "deb [arch=amd64] http://dl.bintray.com/apache/arrow/ubuntu %s main"',
  'apt-get install libarrow-dev libparquet-dev',
  sep = "\n"
)

DEBIAN_INSTALL <- paste(
  'curl --output /usr/share/keyrings/apache-arrow-keyring.gpg https://dl.bintray.com/apache/arrow/%s/apache-arrow-keyring.gpg',
  'tee /etc/apt/sources.list.d/apache-arrow.list <<APT_LINE',
  'deb [arch=amd64 signed-by=/usr/share/keyrings/apache-arrow-keyring.gpg] https://dl.bintray.com/apache/arrow/%s/ %s main',
  'deb-src [signed-by=/usr/share/keyrings/apache-arrow-keyring.gpg] https://dl.bintray.com/apache/arrow/%s/ %s main',
  'APT_LINE',
  'apt update',
  'apt install -y -V libarrow-dev libparquet-dev',
  sep = "\n"
)

DEBIAN_BACKPORT <- paste(
  'tee /etc/apt/sources.list.d/backports.list <<APT_LINE',
  'deb http://deb.debian.org/debian %s-backports main',
  'APT_LINE',
  sep = "\n"
)

CENTOS_INSTALL <- paste(
  'tee /etc/yum.repos.d/Apache-Arrow.repo <<REPO',
  '[apache-arrow]',
  'name=Apache Arrow',
  'baseurl=https://dl.bintray.com/apache/arrow/centos/\\$releasever/\\$basearch/',
  'gpgcheck=1',
  'enabled=1',
  'gpgkey=https://dl.bintray.com/apache/arrow/centos/RPM-GPG-KEY-apache-arrow',
  'REPO',
  'yum install -y epel-release',
  'yum install -y --enablerepo=epel arrow-devel parquet-devel',
  sep = "\n"
)
