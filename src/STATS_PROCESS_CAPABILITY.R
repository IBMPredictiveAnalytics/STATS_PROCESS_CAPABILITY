#/***********************************************************************
# * Licensed Materials - Property of IBM
# * IBM SPSS Products: Statistics Common
# * (C) Copyright IBM Corp. 2026
# ************************************************************************/

# ══════════════════════════════════════════════════════════════════════════════
# PROCESS CAPABILITY ANALYSIS — R EXTENSION FOR IBM SPSS STATISTICS
# Author  : Aruna Saraswathy
# Version : 3.0.0  (Phase 2)
# Date    : 2026-06-06
#
# PHASE 2 ADDITIONS
#   Within vs Overall capability (Cp/Cpk via within-subgroup SD, Pp/Ppk overall)
#   Non-normal capability via Box-Cox transformation (lambda auto-search)
#   Benchmark Sigma with 1.5σ Six Sigma shift (LT vs ST sigma comparison)
#   Process Capability Report Card (RAG grading vs standard SPC thresholds)
#   Multi-variable overlay analysis (multiple variables on shared charts)
#   Enhanced capability histogram (all indices displayed, standard SPC-style)
#   Spec limits displayed on I-MR and all control charts
# ══════════════════════════════════════════════════════════════════════════════



# ── Windows temp-dir fix: ensures R charts are readable by SPSS on all accounts
# Tries C:/Temp first; falls back to C:/Users/Public/Temp; then user profile
if (.Platform$OS.type == "windows") {
    .td_candidates <- c(
        "C:/Temp",
        file.path(Sys.getenv("PUBLIC"),      "Temp"),
        file.path(Sys.getenv("USERPROFILE"), "RTemp")
    )
    for (.td in .td_candidates) {
        if (!dir.exists(.td)) dir.create(.td, recursive=TRUE, showWarnings=FALSE)
        if (dir.exists(.td)) { options(tmpdir = .td); break }
    }
    rm(.td_candidates, .td)
}

# ══════════════════════════════════════════════════════════════════════════════
# PACKAGE INSTALLATION — OneDrive / SharePoint backed local mirror
# Flow:
#   1. All packages already installed? → silent skip
#   2. Local extracted folder exists?  → install from it
#   3. Try download from SharePoint     → extract → install
#   4. Download fails (not signed in)?  → stop with clear user message
# ══════════════════════════════════════════════════════════════════════════════

.SPSS_FALLBACK_ZIP  <- paste0(
    "https://nestle.sharepoint.com/:u:/r/sites/SPSS_Statistics/",
    "Shared%20Documents/Packages/spss_cran_mirror.zip",
    "?csf=1&web=1&e=RHeShh"
)
.SPSS_LOCAL_REPO    <- "C:/spss_cran_mirror"
.SPSS_CRAN_MIRROR   <- "https://cloud.r-project.org"
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

.spss_ensure_packages <- function(pkgs) {
    missing_pkgs <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
    if (length(missing_pkgs) == 0) return(invisible(TRUE))   # all installed

    r_ver    <- paste0(R.version$major, ".",
                       strsplit(R.version$minor, "\\.")[[1]][1])
    repo_dir <- file.path(.SPSS_LOCAL_REPO, "bin", "windows", "contrib", r_ver)
    repo_url <- paste0("file:///", .SPSS_LOCAL_REPO)

    # Helper: check if a package .zip exists in the local repo folder
    pkg_in_repo <- function(pkg) {
        dir.exists(repo_dir) &&
        length(list.files(repo_dir, pattern = paste0("^", pkg, "_"),
                          ignore.case = TRUE)) > 0
    }

    # -- Step 1: try public CRAN (works for any user with internet access) -----
    cran_ok <- tryCatch({
        tmp_chk <- tempfile()
        old_to  <- getOption("timeout"); options(timeout = 15)
        on.exit({ unlink(tmp_chk); options(timeout = old_to) }, add = TRUE)
        res <- suppressWarnings(download.file(
            paste0(.SPSS_CRAN_MIRROR, "/src/contrib/PACKAGES.gz"),
            tmp_chk, quiet = TRUE, mode = "wb"))
        res == 0
    }, error = function(e) FALSE, warning = function(w) FALSE)

    if (cran_ok) {
        # Find a writable R library path — on Mac/Linux the default may be system-owned
        # (read-only), causing silent install failures even when CRAN is reachable.
        writable_lib <- tryCatch({
            candidate <- NULL
            for (p in .libPaths()) {
                if (dir.exists(p) && file.access(p, 2L) == 0L) { candidate <- p; break }
            }
            if (is.null(candidate)) {
                # No writable path — create a personal library
                ulib <- file.path(path.expand("~"), ".R", "library")
                dir.create(ulib, recursive = TRUE, showWarnings = FALSE)
                if (dir.exists(ulib)) {
                    .libPaths(c(ulib, .libPaths()))
                    candidate <- ulib
                }
            }
            candidate
        }, error = function(e) NULL)

        # If SPSS is using renv (SPSS 32+), use renv::install() which is the
        # correct API for renv-managed environments. Fall back to install.packages.
        use_renv <- isNamespaceLoaded("renv") &&
                    tryCatch(is.function(renv::install), error=function(e) FALSE)

        for (pkg in missing_pkgs) {
            if (use_renv) {
                tryCatch(
                    renv::install(pkg, repos = .SPSS_CRAN_MIRROR, prompt = FALSE),
                    error = function(e) NULL
                )
            }
            # Always also try standard install.packages (covers non-renv and as fallback)
            if (!requireNamespace(pkg, quietly = TRUE)) {
                tryCatch(
                    suppressWarnings(
                    install.packages(pkg, repos = .SPSS_CRAN_MIRROR,
                                     lib   = writable_lib,
                                     dependencies = c("Depends","Imports","LinkingTo"),
                                     quiet = TRUE,
                                     type = if (.Platform$OS.type == "windows") "binary"
                                          else if (grepl("x86_64", R.version$arch %||% ""))
                                              "mac.binary.big-sur-x86_64"
                                          else getOption("pkgType"))),
                    error = function(e) NULL
                )
            }
        }

        # Intel Mac extra fallback: still-missing packages installed directly
        # to SPSS rpackage dir (bypasses renv) with explicit binary type.
        if (.Platform$OS.type != "windows" && grepl("x86_64", R.version$arch %||% "")) {
            still_missing2 <- missing_pkgs[!sapply(missing_pkgs, requireNamespace, quietly=TRUE)]
            if (length(still_missing2) > 0) {
                spss_rpkg <- Filter(function(p) grepl("rpackage", p, fixed=TRUE) &&
                                                dir.exists(p) && file.access(p, 2L) == 0L,
                                    .libPaths())
                if (length(spss_rpkg) > 0) {
                    for (pkg in still_missing2) {
                        tryCatch(
                            suppressWarnings(
                            install.packages(pkg, repos = .SPSS_CRAN_MIRROR,
                                             lib  = spss_rpkg[1],
                                             dependencies = c("Depends","Imports","LinkingTo"),
                                             quiet = TRUE,
                                             type  = "mac.binary.big-sur-x86_64")),
                            error = function(e) NULL
                        )
                    }
                }
            }
        }

        # Ensure the writable lib is on the search path before checking
        if (!is.null(writable_lib) && !(writable_lib %in% .libPaths()))
            .libPaths(c(writable_lib, .libPaths()))
        missing_pkgs <- missing_pkgs[!sapply(missing_pkgs, requireNamespace, quietly = TRUE)]
        if (length(missing_pkgs) == 0) return(invisible(TRUE))
    }

    # -- Step 2: CRAN blocked -> try organisation fallback mirror (Windows) ----
    # Download only if any missing package is absent from the local repo.
    # This means a second extension can reuse the existing folder without
    # re-downloading, but will trigger a fresh download if its packages are new.
    if (.Platform$OS.type == "windows") {
        need_download <- !all(sapply(missing_pkgs, pkg_in_repo))

        if (need_download) {
            zip_dest <- file.path(tempdir(), "spss_cran_mirror.zip")
            if (!dir.exists(.SPSS_LOCAL_REPO))
                dir.create(.SPSS_LOCAL_REPO, recursive = TRUE, showWarnings = FALSE)

            dl_ok <- tryCatch({
                suppressWarnings(
                    download.file(.SPSS_FALLBACK_ZIP, zip_dest,
                                  mode = "wb", quiet = TRUE))
                file.size(zip_dest) > 100000 &&
                    !inherits(tryCatch(unzip(zip_dest, list = TRUE),
                                       error = function(e) e), "error")
            }, error = function(e) FALSE)

            if (dl_ok) {
                message("[SPSS] Installing R packages from local mirror...")
                unzip(zip_dest, exdir = .SPSS_LOCAL_REPO, overwrite = TRUE)
                unlink(zip_dest)
            }
        }
    }

    if (dir.exists(repo_dir)) {
        for (pkg in missing_pkgs) {
            tryCatch(
                install.packages(pkg, repos = repo_url, type = "win.binary",
                                 dependencies = TRUE, quiet = TRUE),
                error = function(e) NULL
            )
        }
        missing_pkgs <- missing_pkgs[!sapply(missing_pkgs, requireNamespace, quietly = TRUE)]
        if (length(missing_pkgs) == 0) return(invisible(TRUE))
    }

    # -- Step 3: both failed -> generic message --------------------------------
    stop(paste0(
        "\n\n",
        "================================================================\n",
        "  STATS_PROCESS_CAPABILITY: Required R Packages Missing\n",
        "================================================================\n",
        "The following R packages could not be installed automatically:\n",
        "  ", paste(missing_pkgs, collapse = ", "), "\n\n",
        "Possible causes:\n",
        "  - No internet connection\n",
        "  - Firewall blocking access to cloud.r-project.org\n",
        "  - R package repository unavailable\n\n",
        "To resolve:\n",
        "  1. Check your internet connection and re-run the extension\n",
        "  2. Ask your IT administrator to whitelist cloud.r-project.org\n",
        "  3. Or ask your administrator to pre-install these R packages:\n",
        "     ", paste(missing_pkgs, collapse = ", "), "\n",
        if (.Platform$OS.type != "windows") paste0(
        "\nMac users: if installation keeps failing, open Terminal and run:\n",
        "  xcode-select --install\n",
        "Then re-run the extension. This installs the compiler needed\n",
        "to build R packages from source.\n") else "",
        "================================================================"
    ))
}



# ══════════════════════════════════════════════════════════════════════════════
# SECTION 1 — UTILITY FUNCTIONS
# ══════════════════════════════════════════════════════════════════════════════

strip_quotes <- function(s) {
    # Strip surrounding double or single quotes from literal-ktype strings
    if (is.null(s) || length(s) == 0 || !nzchar(as.character(s))) return(s)
    gsub('^["\']|["\']$', '', as.character(s))
}

to_bool <- function(x) {
    if (is.null(x) || length(x) == 0) return(FALSE)
    if (is.logical(x)) return(isTRUE(x))
    isTRUE(tolower(as.character(x)) %in% c("yes", "true", "1"))
}

setuplocalization <- function(domain) invisible(NULL)
gtxt  <- function(msg, ...) if (...length() == 0) msg else sprintf(msg, ...)
gtxtf <- function(msg, ...) sprintf(msg, ...)

StartProcedure <- function(procname, omsid) {
    ver <- tryCatch(as.numeric(substr(spsspkg.GetSPSSVersion(), 1, 2)),
                    error = function(e) 19)
    if (ver >= 19) spsspkg.StartProcedure(procname, omsid)
    else           spsspkg.StartProcedure(omsid)
}

# Stateful warning collector — IBM reference pattern
Warn <- function(procname, omsid) {
    lcl <- list2env(list(procname = procname, omsid = omsid,
                         msglist = list(), msgnum = 0L))

    lcl$warn <- function(msg = NULL, dostop = FALSE, inproc = FALSE) {
        if (!is.null(msg)) {
            assign("msgnum", lcl$msgnum + 1L, envir = lcl)
            m <- lcl$msglist; m[[lcl$msgnum]] <- msg
            assign("msglist", m, envir = lcl)
        }
        if (is.null(msg) || dostop) {
            lcl$display(inproc)
            if (dostop) stop(gtxt("Procedure stopped due to error."), call. = FALSE)
        }
    }

    lcl$display <- function(inproc = FALSE) {
        if (lcl$msgnum == 0L) {
            if (inproc) spsspkg.EndProcedure()
            return()
        }
        procok <- if (!inproc) {
            tryCatch({ StartProcedure(lcl$procname, lcl$omsid); TRUE },
                     error = function(e) FALSE)
        } else TRUE

        if (procok) {
            tbl    <- spss.BasePivotTable("Warnings", "Warnings")
            rowdim <- BasePivotTable.Append(tbl, Dimension.Place.row,
                         gtxt("Message Number"), hideName = FALSE, hideLabels = FALSE)
            for (i in seq_len(lcl$msgnum)) {
                rc <- spss.CellText.String(as.character(i))
                BasePivotTable.SetCategories(tbl, rowdim, rc)
                BasePivotTable.SetCellValue(tbl, rc,
                    spss.CellText.String(lcl$msglist[[i]]))
            }
            spsspkg.EndProcedure()
        } else {
            for (i in seq_len(lcl$msgnum)) message(lcl$msglist[[i]])
        }
    }
    return(lcl)
}


# ══════════════════════════════════════════════════════════════════════════════
# SECTION 2 — AUDIT LOGGING
# ══════════════════════════════════════════════════════════════════════════════

get_audit_user <- function() {
    tryCatch({
        for (v in c(Sys.getenv("AUDIT_USER"), Sys.getenv("LOGNAME"),
                    Sys.getenv("USER"), Sys.getenv("USERNAME"))) {
            if (nchar(trimws(v)) > 0) return(trimws(v))
        }
        u <- Sys.info()["user"]
        if (!is.na(u) && nchar(trimws(u)) > 0) return(trimws(u))
        return("SYSTEM")
    }, error = function(e) "SYSTEM")
}

log_audit_event <- function(action, details, path = NULL) {
    tryCatch({
        if (is.null(path))
            path <- file.path(path.expand("~"), "process_capability_audit.csv")
        safe  <- gsub(",", ";", gsub("\n", " ", details))
        entry <- sprintf("%s,%s,%s,%s\n",
                         format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
                         get_audit_user(), action, safe)
        if (!file.exists(path)) cat("timestamp,user,action,details\n", file = path)
        cat(entry, file = path, append = TRUE)
    }, error = function(e) invisible(NULL))
}


# ══════════════════════════════════════════════════════════════════════════════
# SECTION 3 — LOGO HELPER
# ══════════════════════════════════════════════════════════════════════════════

add_logo_to_plot <- function(logo_img, position = "topright", size = 0.08) {
    # Draws logo in the TOP margin of the current panel using xpd=TRUE.
    # xpd=TRUE clips to figure (margin) not plot region — never corrupts layout.
    if (is.null(logo_img)) return(invisible(NULL))
    tryCatch({
        opar <- par(xpd = TRUE)
        on.exit(par(opar), add = TRUE)
        usr <- par("usr"); pw <- usr[2]-usr[1]; ph <- usr[4]-usr[3]
        if (pw <= 0 || ph <= 0) return(invisible(NULL))
        img_h <- if (is.array(logo_img)) dim(logo_img)[1] else nrow(logo_img)
        img_w <- if (is.array(logo_img)) dim(logo_img)[2] else ncol(logo_img)
        if (img_h <= 0 || img_w <= 0) return(invisible(NULL))
        # Logo in top margin — height = 70% of top margin height
        mar <- par("mar"); pin <- par("pin"); csi <- par("csi")
        top_u  <- if (pin[2] > 0) ph / pin[2] * csi * mar[3] else ph * 0.08
        logo_h <- max(ph * 0.05, min(top_u * 0.75, ph * 0.18))
        logo_w <- logo_h * (img_w / img_h)
        # Right-align in top margin
        x2 <- usr[2]; x1 <- x2 - logo_w
        y1 <- usr[4] + top_u * 0.10; y2 <- y1 + logo_h
        rasterImage(logo_img, x1, y1, x2, y2, interpolate = TRUE)
    }, error = function(e) invisible(NULL))
}


# ══════════════════════════════════════════════════════════════════════════════
# SECTION 4 — STATISTICAL HELPER FUNCTIONS
# ══════════════════════════════════════════════════════════════════════════════

# Chart color palette for multi-variable overlay
VAR_PALETTE <- c("#2980B9", "#E74C3C", "#27AE60", "#9B59B6",
                 "#F39C12", "#1ABC9C", "#E67E22", "#8E44AD")

# NULL-safe index formatter: handles NULL, NA, and numeric cleanly
safe_idx <- function(label, v, fmt = "%.4f") {
    val <- if (!is.null(v) && length(v) == 1L && !is.na(v))
               sprintf(fmt, v) else "N/A"
    sprintf("%-5s= %s", label, val)
}

# Pooled within-subgroup standard deviation (for Cp/Cpk within-capability)
compute_within_sd <- function(data, groups) {
    lvls <- unique(groups[!is.na(groups)])
    var_sum <- 0; df_sum <- 0
    for (g in lvls) {
        gd <- data[!is.na(groups) & groups == g]
        ng <- length(gd)
        if (ng >= 2) { var_sum <- var_sum + (ng - 1) * var(gd); df_sum <- df_sum + (ng - 1) }
    }
    if (df_sum > 0) sqrt(var_sum / df_sum) else sd(data)
}

# ---- Control-chart bias-correction constants (AIAG SPC / Montgomery) -------
#
# d2(n): expected value of the relative range (range / sigma) for a sample of
# size n drawn from a normal population. No closed form exists; values below
# are the standard SPC textbook table for n = 2..25 (Montgomery, "Introduction
# to Statistical Quality Control", Appendix VI; identical to the standard published reference table).
# For n > 25 the table is held at the n=25 value (common practitioner
# convention — d2 changes very slowly for large n).
D2_TABLE <- c(
    1.128, 1.693, 2.059, 2.326, 2.534, 2.704, 2.847, 2.970, 3.078, 3.173,
    3.258, 3.336, 3.407, 3.472, 3.532, 3.588, 3.640, 3.689, 3.735, 3.778,
    3.819, 3.858, 3.895, 3.931
)
names(D2_TABLE) <- as.character(2:25)

d2_const <- function(n) {
    n <- round(n)
    if (n < 2) return(NA_real_)
    if (n > 25) return(unname(D2_TABLE["25"]))
    unname(D2_TABLE[as.character(n)])
}

# c4(n): exact closed-form bias-correction constant for the sample standard
# deviation of a normal population, c4(n) = sqrt(2/(n-1)) * Gamma(n/2) /
# Gamma((n-1)/2).  E[s] = c4(n) * sigma, so sigma_hat = s / c4(n).
c4_const <- function(n) {
    n <- round(n)
    if (n < 2) return(NA_real_)
    sqrt(2 / (n - 1)) * gamma(n / 2) / gamma((n - 1) / 2)
}

# ---- Alternative within-subgroup sigma estimators (standard SPC-style options) --
#
# R-bar / d2: classical Shewhart Xbar-R approach — average the subgroup
# ranges, then divide by d2(n) (the traditional default for Xbar-R
# charts).  sigma_hat = Rbar / d2(n).  Subgroups of unequal size are handled
# by computing a size-weighted d2 via the *average* subgroup size (the common
# simplification used when subgroup sizes are nearly equal; AIAG SPC manual).
compute_rbar_sd <- function(data, groups) {
    lvls <- unique(groups[!is.na(groups)])
    ranges <- numeric(0); sizes <- numeric(0)
    for (g in lvls) {
        gd <- data[!is.na(groups) & groups == g]
        ng <- length(gd)
        if (ng >= 2) {
            ranges <- c(ranges, max(gd) - min(gd))
            sizes  <- c(sizes, ng)
        }
    }
    if (length(ranges) == 0) return(NA_real_)
    rbar    <- mean(ranges)
    nbar    <- mean(sizes)
    d2v     <- d2_const(nbar)
    if (!is.finite(d2v) || d2v <= 0) return(NA_real_)
    rbar / d2v
}

# S-bar / c4: average the subgroup standard deviations, then divide by
# c4(n) — the standard bias-correction used for Xbar-S charts (preferred over
# Rbar/d2 when subgroup sizes are larger, n >= ~9, because s is a more
# efficient estimator of sigma than the range).  sigma_hat = Sbar / c4(n).
compute_sbar_sd <- function(data, groups) {
    lvls <- unique(groups[!is.na(groups)])
    sds <- numeric(0); sizes <- numeric(0)
    for (g in lvls) {
        gd <- data[!is.na(groups) & groups == g]
        ng <- length(gd)
        if (ng >= 2) {
            sds   <- c(sds, sd(gd))
            sizes <- c(sizes, ng)
        }
    }
    if (length(sds) == 0) return(NA_real_)
    sbar <- mean(sds)
    nbar <- mean(sizes)
    c4v  <- c4_const(nbar)
    if (!is.finite(c4v) || c4v <= 0) return(NA_real_)
    sbar / c4v
}

# Moving-Range-bar / d2(2): the individuals (I-MR chart) estimator, used when
# data are ungrouped/individual measurements. sigma_hat = MRbar / d2(2), with
# d2(2) = 1.128 (the constant for a "subgroup" of two consecutive points —
# the classical Western Electric / AIAG individuals-chart convention,
# identical to the constant conventionally used for I-MR capability sigma).
compute_mrbar_sd <- function(data) {
    x <- data[!is.na(data)]
    n <- length(x)
    if (n < 2) return(NA_real_)
    mr    <- abs(diff(x))
    mrbar <- mean(mr)
    if (!is.finite(mrbar) || mrbar <= 0) return(NA_real_)   # constant data guard
    mrbar / D2_TABLE["2"]
}

# Box-Cox lambda search: maximise the profile log-likelihood under normality
# (the classical Box & Cox 1964 MLE criterion — the same one commonly used in industry SPC software and
# MASS::boxcox use), rather than an ad-hoc "best normality test p-value"
# heuristic. This gives parity with the classical Box-Cox MLE estimate.
#
#   L(λ) = -(n/2)*log( σ̂²(λ) ) + (λ-1) * Σ log(x_i)
#   where y_i(λ) = (x_i^λ - 1)/λ   (λ ≠ 0)   or   log(x_i)   (λ = 0)
#         σ̂²(λ)  = mean( (y_i(λ) - ȳ(λ))² )      [MLE / population variance]
#
# Searched on a fine grid over [-2, 2] (the conventional default search window),
# matching the classical Box-Cox estimator exactly up to grid resolution.
boxcox_lambda <- function(data) {
    data <- data[!is.na(data) & data > 0]
    n <- length(data)
    if (n < 4) return(1)
    log_x     <- log(data)
    sum_log_x <- sum(log_x)

    profile_loglik <- function(lam) {
        y <- if (abs(lam) < 1e-8) log_x else (data^lam - 1) / lam
        if (any(!is.finite(y))) return(-Inf)
        sigma2 <- mean((y - mean(y))^2)
        if (!is.finite(sigma2) || sigma2 <= 0) return(-Inf)
        -(n/2) * log(sigma2) + (lam - 1) * sum_log_x
    }

    best_ll <- -Inf; best_lam <- 1
    for (lam in seq(-2, 2, by = 0.01)) {
        ll <- profile_loglik(lam)
        if (is.finite(ll) && ll > best_ll) { best_ll <- ll; best_lam <- lam }
    }
    best_lam
}

# Apply Box-Cox transform to a value (data or limit)
bc_transform <- function(x, lambda, shift = 0) {
    x <- x + shift
    if (any(x <= 0, na.rm = TRUE)) return(rep(NA_real_, length(x)))
    if (abs(lambda) < 1e-4) log(x) else (x^lambda - 1) / lambda
}

# Capability grade: returns list(grade, colour, action)
grade_cap <- function(v) {
    if (is.na(v)) return(list(grade = "N/A",          col = "#95A5A6",
                              action = "No spec limit"))
    if (v >= 1.67) return(list(grade = "Excellent",   col = "#1E8449",
                               action = "World class — maintain"))
    if (v >= 1.33) return(list(grade = "Capable",     col = "#27AE60",
                               action = "Meets standard — sustain"))
    if (v >= 1.00) return(list(grade = "Marginal",    col = "#D68910",
                               action = "Improvement needed"))
    return(         list(grade = "Not Capable", col = "#C0392B",
                         action = "Immediate action required"))
}



# ══════════════════════════════════════════════════════════════════════════════
# SECTION 4b — DATA DISTRIBUTION ADVISOR
# ══════════════════════════════════════════════════════════════════════════════

# ── Custom distribution density / CDF functions ───────────────────────────────

# Smallest Extreme Value (SEV / Gumbel min): X = min of n normals (log-scale)
dsev_ <- function(x, mu=0, sigma=1, log=FALSE) {
    z  <- (x - mu) / sigma
    ld <- -log(sigma) + z - exp(z)
    if (log) ld else exp(ld)
}
psev_ <- function(q, mu=0, sigma=1, lower.tail=TRUE) {
    p <- 1 - exp(-exp((q - mu)/sigma))
    if (lower.tail) p else 1 - p
}

# Largest Extreme Value (LEV / Gumbel max): X = max of n normals (log-scale)
dlev_ <- function(x, mu=0, sigma=1, log=FALSE) {
    z  <- -(x - mu) / sigma
    ld <- -log(sigma) + z - exp(z)
    if (log) ld else exp(ld)
}
plev_ <- function(q, mu=0, sigma=1, lower.tail=TRUE) {
    p <- exp(-exp(-(q - mu)/sigma))
    if (lower.tail) p else 1 - p
}

# Loglogistic (2P): shape=beta, scale=alpha; CDF = 1/(1+(x/alpha)^(-beta))
dllogis_ <- function(x, shape=1, scale=1, log=FALSE) {
    x  <- pmax(x, .Machine$double.eps)
    z  <- (x/scale)^shape
    ld <- log(shape) - log(scale) + (shape-1)*log(x/scale) - 2*log1p(z)
    if (log) ld else exp(ld)
}
pllogis_ <- function(q, shape=1, scale=1, lower.tail=TRUE) {
    q <- pmax(q, .Machine$double.eps)
    p <- 1 / (1 + (q/scale)^(-shape))
    if (lower.tail) p else 1 - p
}

# Inverse Gaussian (Wald): parameterised as (mean=mu, shape=lambda)
dinvgauss_ <- function(x, mean=1, shape=1, log=FALSE) {
    x  <- pmax(x, .Machine$double.eps)
    ld <- 0.5*(log(shape) - log(2*pi) - 3*log(x)) -
          shape*(x - mean)^2 / (2*mean^2*x)
    if (log) ld else exp(ld)
}
pinvgauss_ <- function(q, mean=1, shape=1, lower.tail=TRUE) {
    q  <- pmax(q, .Machine$double.eps)
    t1 <- sqrt(shape/q)*(q/mean - 1)
    t2 <- -sqrt(shape/q)*(q/mean + 1)
    p  <- pnorm(t1) + exp(2*shape/mean)*pnorm(t2)
    if (lower.tail) p else 1 - p
}

# ── Anderson-Darling goodness-of-fit ─────────────────────────────────────────

# A² statistic from a vector of CDF values at sorted data
da_ad_stat <- function(Fn_sorted) {
    n  <- length(Fn_sorted)
    Fn <- pmax(1e-12, pmin(1-1e-12, Fn_sorted))
    i  <- seq_len(n)
    -n - sum((2*i - 1)*(log(Fn) + log(1 - rev(Fn)))) / n
}

# Approximate p-value for composite-hypothesis AD test (Stephens 1974)
# Log-linear interpolation in standard breakpoint table
da_ad_pval <- function(A2, n=Inf) {
    if (is.na(A2) || !is.finite(A2) || A2 <= 0) return(NA_real_)
    if (is.finite(n) && n > 3)
        A2 <- A2 * (1 + 4/n - 25/n^2)   # finite-n correction
    tbl <- rbind(
        c(0.200,0.999),c(0.243,0.900),c(0.284,0.750),c(0.341,0.500),
        c(0.396,0.250),c(0.481,0.150),c(0.631,0.100),c(0.752,0.050),
        c(1.035,0.025),c(1.159,0.010),c(2.492,0.005),c(3.857,0.001)
    )
    if (A2 <= tbl[1,1]) return(0.999)
    if (A2 >= tbl[nrow(tbl),1]) return(0.0005)
    idx <- findInterval(A2, tbl[,1])
    x0 <- tbl[idx,1]; x1 <- tbl[idx+1,1]
    p0 <- tbl[idx,2]; p1 <- tbl[idx+1,2]
    t  <- (A2 - x0)/(x1 - x0)
    exp(log(p0) + t*(log(p1) - log(p0)))
}

# PP-plot R² (empirical vs theoretical probability quantiles)
da_pp_r2 <- function(dat, pfun, params) {
    tryCatch({
        n  <- length(dat); x <- sort(dat); i <- seq_len(n)
        ep <- (i - 0.375)/(n + 0.25)
        tp <- do.call(pfun, c(list(q=x), as.list(params)))
        if (any(!is.finite(tp))) return(NA_real_)
        cor(ep, tp)^2
    }, error=function(e) NA_real_)
}

# ── Generic MLE via optim() ───────────────────────────────────────────────────

da_fit_mle <- function(dat, dfun, pfun, start, lower=NULL, upper=NULL) {
    n <- length(dat)
    negll <- function(theta) {
        pars <- setNames(as.list(theta), names(start))
        lv   <- tryCatch(do.call(dfun, c(list(x=dat), pars, list(log=TRUE))),
                         error=function(e) rep(-Inf, n))
        if (any(!is.finite(lv))) return(1e15)
        -sum(lv)
    }
    opt <- tryCatch({
        if (!is.null(lower) || !is.null(upper))
            optim(unlist(start), negll, method="L-BFGS-B",
                  lower=if(is.null(lower)) rep(-Inf,length(start)) else lower,
                  upper=if(is.null(upper)) rep( Inf,length(start)) else upper,
                  control=list(maxit=3000))
        else
            optim(unlist(start), negll, method="Nelder-Mead",
                  control=list(maxit=3000, reltol=1e-10))
    }, error=function(e) NULL)
    if (is.null(opt) || opt$convergence > 1) return(NULL)
    params  <- setNames(opt$par, names(start))
    loglik  <- -opt$value
    if (!is.finite(loglik)) return(NULL)
    k       <- length(params)
    Fn_sort <- tryCatch({
        xs <- sort(dat)
        do.call(pfun, c(list(q=xs), as.list(params)))
    }, error=function(e) NULL)
    ad  <- if (!is.null(Fn_sort) && all(is.finite(Fn_sort)))
               da_ad_stat(Fn_sort) else NA_real_
    adp <- da_ad_pval(ad, n)
    r2  <- da_pp_r2(dat, pfun, params)
    list(params=params, loglik=loglik,
         aic=-2*loglik+2*k, bic=-2*loglik+k*log(n),
         ad=ad, ad_p=adp, pp_r2=r2, nparams=k)
}

# ── Density evaluation for gallery plots ─────────────────────────────────────

da_density_vals <- function(fi, x_seq) {
    tryCatch({
        xs <- if (!is.null(fi$loc3)) x_seq - fi$loc3 else x_seq
        p  <- fi$params
        vals <- switch(fi$name,
            "Normal"      = dnorm(x_seq, p["mean"],     p["sd"]),
            "Logistic"    = dlogis(x_seq, p["location"], p["scale"]),
            "Cauchy"      = dcauchy(x_seq, p["location"],p["scale"]),
            "SEV"         = dsev_(x_seq, p["mu"],       p["sigma"]),
            "LEV"         = dlev_(x_seq, p["mu"],       p["sigma"]),
            "Lognormal"   = dlnorm(pmax(xs,1e-15), p["meanlog"],p["sdlog"]),
            "Weibull"     = dweibull(pmax(xs,1e-15), p["shape"],p["scale"]),
            "Gamma"       = dgamma(pmax(xs,1e-15),  p["shape"],p["rate"]),
            "Exponential" = dexp(pmax(xs,1e-15),    p["rate"]),
            "Loglogistic" = dllogis_(pmax(xs,1e-15),p["shape"],p["scale"]),
            "InvGaussian" = dinvgauss_(pmax(xs,1e-15),p["mean"],p["shape"]),
            "Beta"        = dbeta(pmax(pmin(xs,1-1e-9),1e-9),p["shape1"],p["shape2"]),
            "JohnsonSU"   = djohnson_su_(x_seq, p["gamma"], p["delta"], p["xi"], p["lambda"]),
            "JohnsonSB"   = djohnson_sb_(x_seq, p["gamma"], p["delta"], p["xi"], p["lambda"]),
            rep(NA_real_, length(x_seq))
        )
        vals
    }, error=function(e) rep(NA_real_, length(x_seq)))
}

# Format fitted parameters as a readable string
da_fmt_params <- function(fi) {
    p     <- fi$params
    parts <- mapply(function(nm,val) sprintf("%s=%.4g",nm,val), names(p), p)
    txt   <- paste(parts, collapse=", ")
    if (!is.null(fi$loc3) && is.finite(fi$loc3))
        txt <- paste0("loc=",sprintf("%.4g",fi$loc3),", ",txt)
    txt
}

# ── Johnson distribution helpers (pure R, no extra packages) ─────────────────
djohnson_su_ <- function(x, gamma, delta, xi, lambda, log=FALSE) {
    z  <- (x - xi) / lambda
    y  <- gamma + delta * asinh(z)
    ld <- log(delta) - log(abs(lambda)) - 0.5*log(2*pi) -
          0.5*log1p(z^2) - 0.5*y^2
    if (log) ld else exp(ld)
}
pjohnson_su_ <- function(q, gamma, delta, xi, lambda)
    pnorm(gamma + delta * asinh((q - xi) / lambda))

djohnson_sb_ <- function(x, gamma, delta, xi, lambda, log=FALSE) {
    ld <- rep(-Inf, length(x))
    v  <- x > xi & x < xi + lambda
    if (any(v)) {
        xv <- x[v]; zv <- (xv - xi) / (xi + lambda - xv)
        yv <- gamma + delta * log(zv)
        ld[v] <- log(delta) + log(lambda) - log(xi + lambda - xv) -
                 log(xv - xi) - 0.5*log(2*pi) - 0.5*yv^2
    }
    if (log) ld else { r <- exp(ld); r[!is.finite(r)] <- 0; r }
}
pjohnson_sb_ <- function(q, gamma, delta, xi, lambda) {
    p <- rep(0.0, length(q))
    v <- q > xi & q < xi + lambda
    if (any(v))
        p[v] <- pnorm(gamma + delta * log((q[v]-xi)/(xi+lambda-q[v])))
    p[q >= xi + lambda] <- 1
    p
}

# ── Jarque-Bera normality test (base R only) ─────────────────────────────────
# Uses BIASED moments (divide by n throughout) — the original Bera-Jarque (1981)
# formula. Do NOT use sd() which divides by n-1: that inflates the statistic
# by up to 40% on small samples (n<100) and produces wrong p-values.
da_jb_test <- function(x) {
    n <- length(x)
    if (n < 8) return(list(stat=NA_real_, p=NA_real_))
    mu <- mean(x)
    m2 <- mean((x-mu)^2)          # biased variance  (n denominator)
    m3 <- mean((x-mu)^3)
    m4 <- mean((x-mu)^4)
    s3 <- m3 / m2^1.5             # biased skewness
    s4 <- m4 / m2^2               # biased kurtosis
    jb <- n/6 * (s3^2 + (s4-3)^2/4)
    list(stat=jb, p=pchisq(jb, df=2, lower.tail=FALSE))
}

# ── DA CDF helper (for distribution-based capability) ────────────────────────
da_pval <- function(fi, x) {
    p  <- fi$params
    x2 <- if (!is.null(fi$loc3)) x - fi$loc3 else x
    tryCatch(switch(fi$name,
        "Normal"      = pnorm(x,    p["mean"],     p["sd"]),
        "Logistic"    = plogis(x,   p["location"], p["scale"]),
        "Cauchy"      = pcauchy(x,  p["location"], p["scale"]),
        "SEV"         = psev_(x,    p["mu"],       p["sigma"]),
        "LEV"         = plev_(x,    p["mu"],       p["sigma"]),
        "Lognormal"   = , "Lognormal3P" = plnorm(pmax(x2,1e-15), p["meanlog"], p["sdlog"]),
        "Weibull"     = , "Weibull3P"   = pweibull(pmax(x2,1e-15), p["shape"], p["scale"]),
        "Gamma"       = , "Gamma3P"     = pgamma(pmax(x2,1e-15),   p["shape"], p["rate"]),
        "Exponential" = pexp(pmax(x2,1e-15), p["rate"]),
        "Loglogistic" = pllogis_(pmax(x2,1e-15), p["shape"], p["scale"]),
        "InvGaussian" = pinvgauss_(pmax(x2,1e-15), p["mean"], p["shape"]),
        "Beta"        = pbeta(pmax(pmin(x,1-1e-9),1e-9), p["shape1"], p["shape2"]),
        "JohnsonSU"   = pjohnson_su_(x, p["gamma"], p["delta"], p["xi"], p["lambda"]),
        "JohnsonSB"   = pjohnson_sb_(x, p["gamma"], p["delta"], p["xi"], p["lambda"]),
        NA_real_), error=function(e) NA_real_)
}
da_qval <- function(fi, prob, dat) {
    # Numerical CDF inversion via uniroot
    rng <- diff(range(dat, na.rm=TRUE))
    lo  <- min(dat, na.rm=TRUE) - rng * 5
    hi  <- max(dat, na.rm=TRUE) + rng * 5
    tryCatch(uniroot(function(x) da_pval(fi, x) - prob,
                     interval=c(lo, hi), tol=1e-8, extendInt="yes")$root,
             error=function(e) NA_real_)
}

# ── Fit all applicable distributions ─────────────────────────────────────────

da_fit_all <- function(dat, alpha=0.05, include_3p=TRUE) {
    dat  <- dat[is.finite(dat)]
    n    <- length(dat)
    if (n < 5) return(NULL)
    mn   <- mean(dat); vr <- var(dat); sd_ <- sqrt(max(vr, 1e-30))
    mn_  <- min(dat);  mx_ <- max(dat)
    all_pos <- mn_ > 0
    all_01  <- mn_ > 0 && mx_ < 1

    results <- list()

    add_fit <- function(fi, name, label, loc3=NULL) {
        if (is.null(fi)) return()
        fi$name <- name; fi$label <- label; fi$loc3 <- loc3
        results[[length(results)+1]] <<- fi
    }

    # ── Symmetric / unbounded distributions ──────────────────────────────────
    # 1. Normal
    ll_n <- sum(dnorm(dat, mn, sd_, log=TRUE))
    add_fit(list(params=c(mean=mn, sd=sd_), loglik=ll_n,
                 aic=-2*ll_n+4, bic=-2*ll_n+2*log(n),
                 ad=da_ad_stat(pnorm(sort(dat),mn,sd_)),
                 ad_p=da_ad_pval(da_ad_stat(pnorm(sort(dat),mn,sd_)),n),
                 pp_r2=da_pp_r2(dat,pnorm,c(mean=mn,sd=sd_)),
                 nparams=2L),
            "Normal","Normal")

    # 2. Logistic
    add_fit(da_fit_mle(dat, dlogis, plogis,
                       list(location=mn, scale=sd_*sqrt(3)/pi),
                       lower=c(-Inf,1e-6)),
            "Logistic","Logistic")

    # 3. Cauchy (use median/IQR as robust starting values)
    med_ <- median(dat); iqr_ <- IQR(dat)
    add_fit(da_fit_mle(dat, dcauchy, pcauchy,
                       list(location=med_, scale=max(iqr_/2, 1e-4)),
                       lower=c(-Inf,1e-6)),
            "Cauchy","Cauchy")

    # 4. SEV — Smallest Extreme Value (Gumbel min)
    mu_sev <- mn - 0.5772*sd_*sqrt(6)/pi
    sg_sev <- sd_*sqrt(6)/pi
    add_fit(da_fit_mle(dat, dsev_, psev_,
                       list(mu=mu_sev, sigma=max(sg_sev,1e-4)),
                       lower=c(-Inf,1e-6)),
            "SEV","SEV (Gumbel Min)")

    # 5. LEV — Largest Extreme Value (Gumbel max)
    add_fit(da_fit_mle(dat, dlev_, plev_,
                       list(mu=mn + 0.5772*sd_*sqrt(6)/pi,
                            sigma=max(sg_sev,1e-4)),
                       lower=c(-Inf,1e-6)),
            "LEV","LEV (Gumbel Max)")

    # 6. Johnson SU (unbounded, flexible skewness+kurtosis, 4-param)
    tryCatch(
        add_fit(da_fit_mle(dat, djohnson_su_, pjohnson_su_,
                           list(gamma=0, delta=1, xi=mn, lambda=sd_),
                           lower=c(-Inf, 1e-4, -Inf, 1e-4)),
                "JohnsonSU", "Johnson SU"),
        error=function(e) NULL)

    # 7. Johnson SB (bounded, 4-param — only when n ≥ 10 and range finite)
    if (n >= 10 && is.finite(mn_) && is.finite(mx_)) {
        mg   <- (mx_ - mn_) * 0.15
        xi0  <- mn_ - mg
        lam0 <- (mx_ - mn_) + 2*mg
        tryCatch(
            add_fit(da_fit_mle(dat, djohnson_sb_, pjohnson_sb_,
                               list(gamma=0, delta=1, xi=xi0, lambda=lam0),
                               lower=c(-Inf, 1e-4, -Inf, 1e-4)),
                    "JohnsonSB", "Johnson SB"),
            error=function(e) NULL)
    }

    # ── Strictly-positive distributions ──────────────────────────────────────
    if (all_pos) {
        # 6. Lognormal (2P) — closed-form MLE
        ld <- log(dat); lmn <- mean(ld); lsd <- sd(ld)
        ll_ln <- sum(dlnorm(dat,lmn,lsd,log=TRUE))
        add_fit(list(params=c(meanlog=lmn,sdlog=lsd), loglik=ll_ln,
                     aic=-2*ll_ln+4, bic=-2*ll_ln+2*log(n),
                     ad=da_ad_stat(plnorm(sort(dat),lmn,lsd)),
                     ad_p=da_ad_pval(da_ad_stat(plnorm(sort(dat),lmn,lsd)),n),
                     pp_r2=da_pp_r2(dat,plnorm,c(meanlog=lmn,sdlog=lsd)),
                     nparams=2L),
                "Lognormal","Lognormal (2P)")

        # 7. Weibull (2P)
        add_fit(da_fit_mle(dat, dweibull, pweibull,
                           list(shape=max(mn^2/vr,0.5), scale=mn),
                           lower=c(1e-3,1e-6)),
                "Weibull","Weibull (2P)")

        # 8. Gamma (2P) — method-of-moments start
        add_fit(da_fit_mle(dat, dgamma, pgamma,
                           list(shape=max(mn^2/vr,0.5), rate=max(mn/vr,1e-6)),
                           lower=c(1e-3,1e-9)),
                "Gamma","Gamma (2P)")

        # 9. Exponential (1P)
        add_fit(da_fit_mle(dat, dexp, pexp,
                           list(rate=1/mn), lower=c(1e-9)),
                "Exponential","Exponential")

        # 10. Loglogistic (2P)
        add_fit(da_fit_mle(dat, dllogis_, pllogis_,
                           list(shape=max(pi/sd_*mn,0.5), scale=mn),
                           lower=c(1e-3,1e-9)),
                "Loglogistic","Loglogistic (2P)")

        # 11. Inverse Gaussian
        add_fit(da_fit_mle(dat, dinvgauss_, pinvgauss_,
                           list(mean=mn, shape=max(mn^3/vr,1e-3)),
                           lower=c(1e-6,1e-6)),
                "InvGaussian","Inverse Gaussian")
    }

    # ── (0,1)-bounded ─────────────────────────────────────────────────────────
    if (all_01) {
        al <- mn*(mn*(1-mn)/vr - 1); be <- (1-mn)*(mn*(1-mn)/vr - 1)
        if (is.finite(al) && al > 0 && is.finite(be) && be > 0)
            add_fit(da_fit_mle(dat, dbeta, pbeta,
                               list(shape1=al, shape2=be),
                               lower=c(1e-3,1e-3)),
                    "Beta","Beta")
    }

    # ── 3-parameter (threshold) variants ─────────────────────────────────────
    if (include_3p && all_pos && n >= 10) {
        theta <- mn_ * 0.95   # location shift: 95% of minimum

        # 13. Weibull (3P)
        d3 <- dat - theta
        if (all(d3 > 0)) {
            mn3 <- mean(d3); vr3 <- var(d3)
            fi3 <- da_fit_mle(d3, dweibull, pweibull,
                              list(shape=max(mn3^2/vr3,0.5), scale=mn3),
                              lower=c(1e-3,1e-6))
            if (!is.null(fi3)) {
                fi3$loc3 <- theta; fi3$name <- "Weibull3P"
                fi3$label <- "Weibull (3P)"; fi3$nparams <- 3L
                fi3$aic <- fi3$aic + 2; fi3$bic <- fi3$bic + log(n)
                results[[length(results)+1]] <- fi3
            }
        }

        # 14. Lognormal (3P)
        if (all(d3 > 0)) {
            ld3 <- log(d3); lm3 <- mean(ld3); ls3 <- sd(ld3)
            ll3 <- sum(dlnorm(d3,lm3,ls3,log=TRUE))
            ad3 <- da_ad_stat(plnorm(sort(d3),lm3,ls3))
            results[[length(results)+1]] <- list(
                name="Lognormal3P", label="Lognormal (3P)",
                params=c(meanlog=lm3, sdlog=ls3), loglik=ll3,
                aic=-2*ll3+6, bic=-2*ll3+3*log(n),
                ad=ad3, ad_p=da_ad_pval(ad3,n),
                pp_r2=da_pp_r2(d3, plnorm, c(meanlog=lm3,sdlog=ls3)),
                nparams=3L, loc3=theta)
        }

        # 15. Gamma (3P)
        if (all(d3 > 0)) {
            mn3 <- mean(d3); vr3 <- var(d3)
            fi3g <- da_fit_mle(d3, dgamma, pgamma,
                               list(shape=max(mn3^2/vr3,0.5), rate=max(mn3/vr3,1e-6)),
                               lower=c(1e-3,1e-9))
            if (!is.null(fi3g)) {
                fi3g$loc3 <- theta; fi3g$name <- "Gamma3P"
                fi3g$label <- "Gamma (3P)"; fi3g$nparams <- 3L
                fi3g$aic <- fi3g$aic + 2; fi3g$bic <- fi3g$bic + log(n)
                results[[length(results)+1]] <- fi3g
            }
        }
    }

    if (length(results) == 0) return(NULL)

    # ── Rank by composite score: 60% AD p-value + 40% AIC rank ──────────────
    aic_vals <- sapply(results, function(r) r$aic)
    aic_rnk  <- rank(aic_vals, ties.method="first")
    m        <- length(results)
    for (i in seq_len(m)) {
        adp_i <- results[[i]]$ad_p
        adp_i <- if (is.na(adp_i)) 0 else adp_i
        aic_s <- 1 - (aic_rnk[i]-1)/(m - 1 + 1e-9)
        results[[i]]$score <- 0.60*adp_i + 0.40*aic_s
    }
    results <- results[order(-sapply(results, function(r) r$score))]
    results
}

# ── Generate full Distribution Advisor HTML ───────────────────────────────────

da_html_section <- function(dat, varlab, lsl_val=NULL, usl_val=NULL,
                             alpha=0.05, include_3p=TRUE, plt_div_fn=NULL,
                             out_env=NULL, sigma_mult=6.0, sigma_half=3.0) {
    tryCatch({
        fits <- da_fit_all(dat, alpha=alpha, include_3p=include_3p)
        if (is.null(fits) || length(fits) == 0) return("")

        n        <- length(dat[is.finite(dat)])
        m_fits   <- length(fits)
        top_n    <- min(6L, m_fits)
        top_fits <- fits[seq_len(top_n)]

        # Color helpers
        pval_col <- function(p) {
            if (is.na(p)) return("#95A5A6")
            if (p >= 0.10) return("#1E8449")
            if (p >= 0.05) return("#D68910")
            "#C0392B"
        }
        pval_bg <- function(p) {
            if (is.na(p)) return("#F8F9FA")
            if (p >= 0.10) return("#D5F5E3")
            if (p >= 0.05) return("#FDEBD0")
            "#FADBD8"
        }

        # ── Recommendation box ─────────────────────────────────────────────────
        best    <- fits[[1]]
        n_pass  <- sum(sapply(fits, function(r) !is.na(r$ad_p) && r$ad_p >= alpha))
        if (!is.na(best$ad_p) && best$ad_p >= alpha) {
            v_col  <- "#1E8449"; v_bg <- "#D5F5E3"; v_bdr <- "#1E8449"
            v_icon <- "&#10003;"
            v_txt  <- sprintf(
                "<strong>%s</strong> is the best-fitting distribution (AD p = %.4f &gt; &alpha;=%.2f). %d distribution%s pass%s the fit test at this threshold.",
                best$label, best$ad_p, alpha, n_pass,
                if(n_pass==1)""else"s", if(n_pass==1)"s"else"")
        } else if (!is.na(best$ad_p)) {
            v_col  <- "#D68910"; v_bg <- "#FDEBD0"; v_bdr <- "#D68910"
            v_icon <- "&#9888;"
            v_txt  <- sprintf(
                "No distribution passes at &alpha;=%.2f. <strong>%s</strong> is best available (AD p = %.4f, AIC = %.1f). Consider data transformation or checking for outliers/multimodality.",
                alpha, best$label, best$ad_p, best$aic)
        } else {
            v_col  <- "#2980B9"; v_bg <- "#D6EAF8"; v_bdr <- "#2980B9"
            v_icon <- "&#9432;"
            v_txt  <- sprintf("Best by AIC: <strong>%s</strong> (AIC = %.1f). AD test not available.",
                               best$label, best$aic)
        }
        box_html <- sprintf(
            '<div style="margin:10px 0 12px;padding:13px 18px;background:%s;border-left:5px solid %s;border-radius:0 6px 6px 0;font-size:13px">
<span style="font-size:18px;color:%s;margin-right:8px;vertical-align:middle">%s</span>
<strong style="color:%s;font-size:13px">Best-Fit Recommendation</strong><br>
<span style="color:#333;font-size:12.5px">%s</span></div>',
            v_bg, v_bdr, v_col, v_icon, v_col, v_txt)

        # ── Ranked table ───────────────────────────────────────────────────────
        tbl_rows <- paste(sapply(seq_along(fits), function(i) {
            fi    <- fits[[i]]
            bg_r  <- if (i%%2==0) "#FAFAFA" else "#FFFFFF"
            bg_p  <- pval_bg(fi$ad_p); c_p <- pval_col(fi$ad_p)
            p_str <- if (is.na(fi$ad_p)) "&mdash;" else sprintf("%.4f", fi$ad_p)
            ad_s  <- if (is.na(fi$ad))   "&mdash;" else sprintf("%.4f", fi$ad)
            r2_s  <- if (is.na(fi$pp_r2))  "&mdash;" else sprintf("%.4f", fi$pp_r2)
            dec   <- if (!is.na(fi$ad_p) && fi$ad_p >= alpha)
                         '<span style="color:#1E8449;font-weight:bold">Pass</span>'
                     else if (!is.na(fi$ad_p))
                         '<span style="color:#C0392B;font-weight:bold">Fail</span>'
                     else "&mdash;"
            rnk   <- if (i==1)
                '<span style="background:#27AE60;color:white;padding:2px 7px;border-radius:3px;font-size:10px;font-weight:bold">&#9733; BEST</span>'
            else as.character(i)
            sprintf(paste0(
                '<tr style="border-bottom:1px solid #ECF0F1;background:%s">',
                '<td style="padding:7px 10px;text-align:center">%s</td>',
                '<td style="padding:7px 10px;font-weight:%s">%s</td>',
                '<td style="padding:7px 10px;text-align:center">%d</td>',
                '<td style="padding:7px 10px;text-align:center;font-family:monospace;font-size:11px">%s</td>',
                '<td style="padding:7px 10px;text-align:center;background:%s">',
                    '<strong style="color:%s;font-size:12px">%s</strong>',
                    '<br><small>%s</small></td>',
                '<td style="padding:7px 10px;text-align:right;font-family:monospace;font-size:11px">%.1f</td>',
                '<td style="padding:7px 10px;text-align:right;font-family:monospace;font-size:11px">%.1f</td>',
                '<td style="padding:7px 10px;text-align:center;font-size:11px">%s</td>',
                '<td style="padding:7px 10px;font-size:10.5px;color:#555;max-width:220px;word-break:break-all">%s</td>',
                '</tr>'),
            bg_r, rnk, if(i==1)"bold"else"normal", fi$label,
            fi$nparams, ad_s,
            bg_p, c_p, p_str, dec,
            fi$aic, fi$bic, r2_s, da_fmt_params(fi))
        }), collapse="")

        tbl_html <- paste0(
            '<div style="overflow-x:auto;margin:6px 0 16px">',
            '<table style="width:100%;border-collapse:collapse;font-size:12px;min-width:700px">',
            '<thead><tr style="background:#2C3E50;color:white">',
            '<th style="padding:9px 10px;text-align:center;white-space:nowrap;width:60px">Rank</th>',
            '<th style="padding:9px 10px;text-align:left;min-width:130px">Distribution</th>',
            '<th style="padding:9px 10px;text-align:center;width:55px">Params</th>',
            '<th style="padding:9px 10px;text-align:center;white-space:nowrap;width:80px">AD Stat</th>',
            '<th style="padding:9px 10px;text-align:center;white-space:nowrap;width:100px">AD p-value</th>',
            '<th style="padding:9px 10px;text-align:right;width:75px">AIC</th>',
            '<th style="padding:9px 10px;text-align:right;width:75px">BIC</th>',
            '<th style="padding:9px 10px;text-align:center;width:65px">PP R²</th>',
            '<th style="padding:9px 10px;text-align:left">Fitted Parameters</th>',
            '</tr></thead><tbody>',
            tbl_rows,
            '</tbody><tfoot><tr>',
            '<td colspan="9" style="padding:7px 10px;font-size:10px;color:#888;font-style:italic;background:#F8F9FA">',
            sprintf('&#9733; Ranked by composite score (60%% AD p-value + 40%% AIC rank). AD p-values: Stephens (1974) asymptotic approximation with finite-n correction. &alpha; = %.2f &bull; n = %d.',
                    alpha, n),
            '</td></tr></tfoot></table></div>')

        # ── Chart 1: Density Fit Gallery (top 6 in 2-col subplot grid) ─────────
        drange <- range(dat, na.rm=TRUE)
        xpad   <- diff(drange)*0.12
        xl_d   <- c(drange[1]-xpad, drange[2]+xpad)
        x_seq  <- seq(xl_d[1], xl_d[2], length.out=300)
        n_bins <- max(5L, min(30L, as.integer(ceiling(2*n^(1/3)))))   # Rice rule

        gal_plots <- lapply(seq_along(top_fits), function(i) {
            fi    <- top_fits[[i]]
            yd    <- da_density_vals(fi, x_seq)
            valid <- is.finite(yd)
            col_i <- pval_col(fi$ad_p)
            p_str <- if (is.na(fi$ad_p)) "AD N/A" else sprintf("p=%.4f", fi$ad_p)
            badge <- if (!is.na(fi$ad_p) && fi$ad_p >= alpha) "✔ PASS" else "✖ FAIL"

            gi <- plotly::plot_ly() %>%
                plotly::add_histogram(
                    x=dat, histnorm="probability density", nbinsx=n_bins,
                    marker=list(color="rgba(174,214,241,0.65)",
                                line=list(color="rgba(93,173,226,0.8)",width=0.7)),
                    showlegend=FALSE, name="Data")
            if (any(valid))
                gi <- plotly::add_trace(gi,
                    x=x_seq[valid], y=yd[valid],
                    type="scatter", mode="lines",
                    line=list(color=col_i, width=2.5),
                    showlegend=FALSE, name=fi$label)

            # Spec limits as scatter traces — yref="paper" shapes break in subplot()
            y_sl <- max(c(hist(dat, plot=FALSE, breaks=n_bins)$density,
                          yd[is.finite(yd)]), na.rm=TRUE) * 1.25
            if (!is.null(lsl_val) && is.finite(lsl_val))
                gi <- plotly::add_trace(gi,
                    x=c(lsl_val, lsl_val), y=c(0, y_sl),
                    type="scatter", mode="lines",
                    line=list(color="#E74C3C", dash="dash", width=1.8),
                    showlegend=FALSE, visible=FALSE, name="LSL")
            if (!is.null(usl_val) && is.finite(usl_val))
                gi <- plotly::add_trace(gi,
                    x=c(usl_val, usl_val), y=c(0, y_sl),
                    type="scatter", mode="lines",
                    line=list(color="#27AE60", dash="dashdot", width=1.8),
                    showlegend=FALSE, visible=FALSE, name="USL")

            plotly::layout(gi,
                title=list(
                    text=sprintf("<b>%d. %s</b><br><span style='font-size:10px;color:%s'>%s &bull; %s</span>",
                                 i, fi$label, col_i, p_str, badge),
                    font=list(size=11, color="#2C3E50"), x=0.5, xanchor="center"),
                xaxis=list(title="", showgrid=TRUE, gridcolor="#F0F0F0", zeroline=FALSE),
                yaxis=list(title=if(i%%2==1)"Density"else"",
                           showgrid=TRUE, gridcolor="#F0F0F0", zeroline=FALSE),
                plot_bgcolor="#FAFAFA", paper_bgcolor="#FFFFFF",
                margin=list(l=44, r=10, t=72, b=28))
        })

        n_cols_g <- 2L; n_rows_g <- ceiling(length(gal_plots)/n_cols_g)
        p_gallery <- tryCatch(
            plotly::subplot(gal_plots, nrows=n_rows_g,
                            shareX=FALSE, shareY=FALSE,
                            titleX=FALSE, titleY=TRUE, margin=0.08),
            error=function(e) gal_plots[[1]])
        p_gallery <- plotly::layout(p_gallery,
            title=list(
                text=sprintf("<b>Distribution Fit Gallery — %s</b>", varlab),
                font=list(size=14, color="#2C3E50"), x=0.5, xanchor="center"),
            showlegend=FALSE,
            plot_bgcolor="#FAFAFA", paper_bgcolor="#FFFFFF")

        # ── Chart 2: Fit Leaderboard (horizontal bar, all distributions) ────────
        ldr_lab <- rev(sapply(fits, function(r) r$label))
        ldr_p   <- rev(sapply(fits, function(r) ifelse(is.na(r$ad_p), 0, r$ad_p)))
        ldr_col <- rev(sapply(fits, function(r) pval_col(r$ad_p)))
        ldr_txt <- rev(sapply(fits, function(r)
            if(is.na(r$ad_p)) "N/A" else sprintf("%.4f", r$ad_p)))

        p_leader <- plotly::plot_ly(
            x=ldr_p, y=ldr_lab, type="bar", orientation="h",
            marker=list(color=ldr_col, line=list(color="white",width=0.5)),
            text=ldr_txt, textposition="outside",
            hovertemplate="<b>%{y}</b><br>AD p-value: %{text}<extra></extra>") %>%
        plotly::layout(
            title=list(
                text=sprintf("<b>Fit Leaderboard — %s</b>  <span style='font-size:11px;color:#888'>(composite score: 60%% AD p + 40%% AIC rank)</span>",
                             varlab),
                font=list(size=14, color="#2C3E50"), x=0.5, xanchor="center"),
            xaxis=list(title="Anderson-Darling p-value",
                       range=c(0, max(c(ldr_p, alpha*1.1), na.rm=TRUE)*1.4),
                       showgrid=TRUE, gridcolor="#F0F0F0", zeroline=FALSE),
            yaxis=list(title="", automargin=TRUE, tickfont=list(size=11)),
            shapes=list(
                list(type="line", x0=alpha, x1=alpha, y0=-0.5, y1=m_fits-0.5,
                     line=list(color="#E74C3C", dash="dash", width=1.5)),
                list(type="line", x0=0.10,  x1=0.10,  y0=-0.5, y1=m_fits-0.5,
                     line=list(color="#27AE60", dash="dot",  width=1.5))),
            annotations=list(
                list(x=alpha, y=m_fits-0.4,
                     text=sprintf("&alpha;=%.2f",alpha), showarrow=FALSE,
                     font=list(size=9,color="#E74C3C"),
                     xanchor="center", yanchor="bottom"),
                list(x=0.10, y=m_fits-0.4,
                     text="0.10", showarrow=FALSE,
                     font=list(size=9,color="#27AE60"),
                     xanchor="center", yanchor="bottom")),
            plot_bgcolor="#FAFAFA", paper_bgcolor="#FFFFFF",
            bargap=0.25, margin=list(l=10, r=65, t=55, b=40))

        # ── Assemble section ───────────────────────────────────────────────────
        gal_h  <- n_rows_g * 275
        ldr_h  <- max(280, m_fits*28 + 90)

        mk_simple_div <- function(p, div_id, h) {
            fj  <- suppressMessages(suppressWarnings(plotly::plotly_json(p, FALSE)))
            cfg <- paste0('{"responsive":true,"displayModeBar":true,',
                          '"displaylogo":false,',
                          '"modeBarButtonsToRemove":["lasso2d","select2d"],',
                          '"toImageButtonOptions":{"format":"png","filename":"',
                          div_id,'","scale":2}}')
            sprintf(paste0(
                '<div style="position:relative">',
                '<div id="%s" style="width:100%%;height:%dpx"></div></div>\n',
                '<script>Plotly.react("%s",%s,{},%s)</script>'),
                div_id, h, div_id, fj, cfg)
        }

        if (!is.null(plt_div_fn)) {
            # has_spec_limits=FALSE: suppress plt_div's built-in LSL/USL button —
            # the DA section has its own dedicated toggle (spec_toggle_html above).
            gal_div <- plt_div_fn(p_gallery,
                                  paste0("plt_da_gal_", make.names(varlab)),
                                  gal_h, "Distribution Fit Gallery",
                                  has_spec_limits=FALSE)
            ldr_div <- plt_div_fn(p_leader,
                                  paste0("plt_da_ldr_", make.names(varlab)),
                                  ldr_h, "Fit Leaderboard",
                                  has_spec_limits=FALSE)
        } else {
            gal_div <- mk_simple_div(p_gallery,
                                     paste0("plt_da_gal_", make.names(varlab)), gal_h)
            ldr_div <- mk_simple_div(p_leader,
                                     paste0("plt_da_ldr_", make.names(varlab)), ldr_h)
        }

        # ── Download buttons (Plotly.toImage — works in SPSS WebView) ─────────────
        gal_id   <- paste0("plt_da_gal_", make.names(varlab))
        ldr_id   <- paste0("plt_da_ldr_", make.names(varlab))
        btn_sty  <- "font-size:11px;padding:4px 10px;border:none;border-radius:3px;cursor:pointer;color:white;"

        # Helper: download a DA Plotly chart as PNG.
        # Uses SVG-DOM extraction (avoids canvas-taint from embedded logo image) with
        # Plotly.toImage as fallback.
        da_mk_dl_btn <- function(div_id, filename, label) {
            # Composites ALL Plotly SVG layers onto one canvas so shapes,
            # annotations, and subplot titles are all included in the PNG.
            # Plotly renders 2 stacked SVGs both classed "main-svg":
            #   [0] axes/bars/traces   [1] shapes/annotations/infolayer
            # We draw them sequentially so both appear in the download.
            js <- paste0(
                "(function(){",
                "var el=document.getElementById('", div_id, "');",
                "if(!el)return;",
                # --- Multi-layer SVG extraction path ---
                "var svgs=el.querySelectorAll('svg.main-svg');",
                "if(svgs&&svgs.length>0){",
                  "try{",
                    "var W=el.offsetWidth*2||800,H=el.offsetHeight*2||400;",
                    "var c=document.createElement('canvas');",
                    "c.width=W;c.height=H;",
                    "var ctx=c.getContext('2d');",
                    "ctx.fillStyle='#fff';ctx.fillRect(0,0,W,H);",
                    "var arr=Array.from(svgs);var idx=0;",
                    "function drawNext(){",
                      "if(idx>=arr.length){",
                        # All layers drawn - trigger download
                        "try{",
                          "var a=document.createElement('a');",
                          "a.href=c.toDataURL('image/png');",
                          "a.download='", filename, ".png';",
                          "document.body.appendChild(a);a.click();document.body.removeChild(a);",
                        "}catch(e){",
                          "Plotly.toImage(el,{format:'png',scale:2})",
                          ".then(function(u){var a=document.createElement('a');",
                          "a.href=u;a.download='", filename, ".png';",
                          "document.body.appendChild(a);a.click();document.body.removeChild(a);})",
                          ".catch(function(){Plotly.downloadImage(el,{format:'png',filename:'", filename, "',scale:2});});",
                        "}",
                        "return;",
                      "}",
                      "var svg=arr[idx++];",
                      "var cl=svg.cloneNode(true);",
                      # Strip <image> nodes (embedded logo) to prevent canvas taint
                      "cl.querySelectorAll('image').forEach(function(n){n.remove();});",
                      "var s=new XMLSerializer().serializeToString(cl);",
                      "var b64=btoa(unescape(encodeURIComponent(s)));",
                      "var img=new Image();",
                      "img.onload=function(){",
                        "try{ctx.drawImage(img,0,0,W,H);}catch(e){}",
                        "drawNext();",
                      "};",
                      "img.onerror=function(){drawNext();};",
                      "img.src='data:image/svg+xml;base64,'+b64;",
                    "}",
                    "drawNext();",
                    "return;",
                  "}catch(e2){}",
                "}",
                # --- Plotly.toImage fallback ---
                "Plotly.toImage(el,{format:'png',scale:2})",
                ".then(function(u){var a=document.createElement('a');",
                "a.href=u;a.download='", filename, ".png';",
                "document.body.appendChild(a);a.click();document.body.removeChild(a);",
                "}).catch(function(){",
                "Plotly.downloadImage(el,{format:'png',filename:'", filename, "',scale:2});",
                "});})();"
            )
            paste0('<div style="text-align:right;margin:4px 0 4px">',
                   '<button style="background:#2C3E50;', btn_sty, '" onclick="', js, '">',
                   label, '</button></div>')
        }
        gal_dl_btn <- da_mk_dl_btn(gal_id, "dist_fit_gallery", "&#11015; Download Gallery PNG")
        ldr_dl_btn <- da_mk_dl_btn(ldr_id, "fit_leaderboard",  "&#11015; Download Leaderboard PNG")

        # ── Gallery LSL/USL toggle button ─────────────────────────────────────────
        has_specs_da <- (!is.null(lsl_val) && is.finite(lsl_val)) ||
                        (!is.null(usl_val) && is.finite(usl_val))
        spec_btn_id  <- paste0("da_specbtn_", make.names(varlab))
        da_toggle_js <- paste0(
            '<script>',
            "function toggleDASpecs_", make.names(varlab), "(divId,btnId){",
            "var gd=document.getElementById(divId);if(!gd||!gd.data)return;",
            "var idxs=[];",
            "gd.data.forEach(function(t,i){if(t.name==='LSL'||t.name==='USL')idxs.push(i);});",
            "if(idxs.length===0)return;",
            "var show=(gd.data[idxs[0]].visible===false);",
            "Plotly.restyle(gd,{visible:show},idxs);",
            "var btn=document.getElementById(btnId);",
            "if(btn){btn.style.background=show?'#8E44AD':'white';",
            "btn.style.color=show?'white':'#8E44AD';",
            "btn.style.border='1px solid #8E44AD';}",
            "}",
            '</script>')
        spec_toggle_html <- if (has_specs_da) paste0(
            da_toggle_js,
            '<div style="text-align:right;margin:0 0 2px">',
            '<button id="', spec_btn_id, '" ',
            "onclick=\"toggleDASpecs_", make.names(varlab), "('", gal_id, "','", spec_btn_id, "')\" ",
            'title="Click to show LSL/USL limits in the fit gallery" ',
            'style="background:white;color:#8E44AD;border:1.5px solid #8E44AD;border-radius:4px;',
            'padding:3px 10px;cursor:pointer;font-size:10px;font-weight:bold">',
            'LSL/USL</button></div>')
        else ""

        # CSV download for Full Rankings table
        csv_hdr  <- "Rank,Distribution,Params,AD Stat,AD p-value,Pass,AIC,BIC,PP R2,Parameters"
        csv_body <- paste(sapply(seq_along(fits), function(i) {
            fi   <- fits[[i]]
            pass <- if (is.na(fi$ad_p)) "N/A" else if (fi$ad_p >= alpha) "Pass" else "Fail"
            paste(i,
                  paste0('"', fi$label, '"'),
                  fi$nparams,
                  if (is.na(fi$ad))    "NA" else sprintf("%.4f", fi$ad),
                  if (is.na(fi$ad_p))  "NA" else sprintf("%.4f", fi$ad_p),
                  pass,
                  sprintf("%.2f", fi$aic),
                  sprintf("%.2f", fi$bic),
                  if (is.na(fi$pp_r2)) "NA" else sprintf("%.4f", fi$pp_r2),
                  paste0('"', gsub('"', '""', da_fmt_params(fi)), '"'),
                  sep=",")
        }), collapse="\n")
        csv_all  <- paste0(csv_hdr, "\n", csv_body)
        csv_uri  <- paste0("data:text/csv;charset=utf-8,",
                           URLencode(csv_all, reserved=TRUE))
        csv_dl_btn <- paste0(
            '<div style="text-align:right;margin:8px 0 4px">',
            '<a href="', csv_uri, '" download="distribution_rankings_',
            make.names(varlab), '.csv" ',
            'style="background:#27AE60;', btn_sty, 'text-decoration:none;display:inline-block">',
            '&#11015; Download Rankings CSV</a></div>')

        # ── Distribution-based capability analysis ────────────────────────────────
        cap_tbl_html <- ""
        cap_df       <- NULL
        if ((!is.null(lsl_val) && is.finite(lsl_val)) ||
            (!is.null(usl_val) && is.finite(usl_val))) {
            tryCatch({
                # Percentile-based Cp/Cpk from best fit distribution CDF
                pct_lsl <- if (!is.null(lsl_val) && is.finite(lsl_val))
                               da_pval(best, lsl_val) else 0
                pct_usl <- if (!is.null(usl_val) && is.finite(usl_val))
                               da_pval(best, usl_val) else 1
                pct_lsl <- max(0, min(pct_lsl, 1))
                pct_usl <- max(0, min(pct_usl, 1))
                pnc_pct  <- (pct_lsl + (1 - pct_usl)) * 1e6  # PPM
                # Equivalent normal z-scores → Cp / Cpk
                z_lsl_pct <- if (!is.null(lsl_val) && is.finite(lsl_val))
                                 abs(qnorm(pct_lsl)) else NA_real_
                z_usl_pct <- if (!is.null(usl_val) && is.finite(usl_val))
                                 abs(qnorm(da_pval(best, usl_val))) else NA_real_
                cpk_pct <- min(z_lsl_pct, z_usl_pct, na.rm=TRUE) / sigma_half
                cp_pct  <- if (!is.null(lsl_val) && !is.null(usl_val) &&
                                is.finite(lsl_val) && is.finite(usl_val)) {
                    q_999 <- da_qval(best, 0.99865, dat)
                    q_001 <- da_qval(best, 0.00135, dat)
                    if (is.finite(q_999) && is.finite(q_001) && q_999 > q_001)
                        (usl_val - lsl_val) / (q_999 - q_001)
                    else NA_real_
                } else NA_real_

                # Transform-based Cp/Cpk: Box-Cox transform data → normal capability
                bc_cp <- NA_real_; bc_cpk <- NA_real_; bc_ppm <- NA_real_
                bc_lambda_used <- NA_real_
                if (all(dat > 0)) tryCatch({
                    # Find best Box-Cox lambda
                    best_ll <- -Inf; best_lam <- 1
                    for (lam in seq(-2, 2, by=0.1)) {
                        xt <- if (abs(lam) < 1e-6) log(dat) else (dat^lam - 1)/lam
                        ll <- sum(dnorm(xt, mean(xt), sd(xt), log=TRUE)) +
                              (lam - 1) * sum(log(dat))
                        if (is.finite(ll) && ll > best_ll) { best_ll <- ll; best_lam <- lam }
                    }
                    bc_lambda_used <- best_lam
                    xt  <- if (abs(best_lam) < 1e-6) log(dat) else (dat^best_lam - 1)/best_lam
                    xmu <- mean(xt); xsd <- sd(xt)
                    lsl_t <- if (!is.null(lsl_val) && is.finite(lsl_val) && lsl_val > 0)
                                 if (abs(best_lam) < 1e-6) log(lsl_val)
                                 else (lsl_val^best_lam - 1)/best_lam else NA_real_
                    usl_t <- if (!is.null(usl_val) && is.finite(usl_val) && usl_val > 0)
                                 if (abs(best_lam) < 1e-6) log(usl_val)
                                 else (usl_val^best_lam - 1)/best_lam else NA_real_
                    bc_cpk <- min((xmu - lsl_t)/(sigma_half*xsd), (usl_t - xmu)/(sigma_half*xsd), na.rm=TRUE)
                    bc_cp  <- if (!is.null(lsl_val) && !is.null(usl_val) &&
                                  is.finite(lsl_val) && is.finite(usl_val) &&
                                  is.finite(lsl_t) && is.finite(usl_t))
                                 (usl_t - lsl_t) / (sigma_mult*xsd) else NA_real_
                    p_below <- if (is.finite(lsl_t)) pnorm(lsl_t, xmu, xsd) else 0
                    p_above <- if (is.finite(usl_t)) 1 - pnorm(usl_t, xmu, xsd) else 0
                    bc_ppm  <- (p_below + p_above) * 1e6
                }, error=function(e) NULL)

                # Build comparison table
                fmt_val <- function(x) if (is.na(x) || !is.finite(x)) "N/A"
                             else sprintf("%.4f", x)
                fmt_ppm <- function(x) if (is.na(x) || !is.finite(x)) "N/A"
                             else sprintf("%.1f", x)
                row_style_h <- "background:#2C3E50;color:white;padding:6px 10px;font-size:12px;font-weight:bold;text-align:center"
                row_style_a <- "padding:6px 10px;font-size:12px;text-align:center;background:#F8F9FA"
                row_style_b <- "padding:6px 10px;font-size:12px;text-align:center"
                cap_tbl_html <- paste0(
                    '<h4 style="color:#2C3E50;margin:16px 0 6px;font-size:13px;',
                    'border-left:4px solid #8E44AD;padding-left:10px">',
                    'Distribution-Based Capability Analysis</h4>',
                    '<table style="width:100%;border-collapse:collapse;margin-bottom:10px;font-size:12px">',
                    '<thead><tr>',
                    '<th style="', row_style_h, '">Method</th>',
                    '<th style="', row_style_h, '">Cp</th>',
                    '<th style="', row_style_h, '">Cpk</th>',
                    '<th style="', row_style_h, '">Est. PPM OOSpec</th>',
                    '<th style="', row_style_h, '">Notes</th>',
                    '</tr></thead><tbody>',
                    '<tr>',
                    '<td style="', row_style_a, '">Percentile (best fit:<br><b>', best$label, '</b>)</td>',
                    '<td style="', row_style_a, '">', fmt_val(cp_pct),  '</td>',
                    '<td style="', row_style_a, '">', fmt_val(cpk_pct), '</td>',
                    '<td style="', row_style_a, '">', fmt_ppm(pnc_pct), '</td>',
                    '<td style="', row_style_a, '">Equivalent-normal Cpk from CDF tail probabilities</td>',
                    '</tr>',
                    '<tr>',
                    '<td style="', row_style_b, '">Box-Cox Transform',
                    if (is.finite(bc_lambda_used)) paste0("<br>(&#955;=", round(bc_lambda_used,2), ")") else "",
                    '</td>',
                    '<td style="', row_style_b, '">', fmt_val(bc_cp),  '</td>',
                    '<td style="', row_style_b, '">', fmt_val(bc_cpk), '</td>',
                    '<td style="', row_style_b, '">', fmt_ppm(bc_ppm), '</td>',
                    '<td style="', row_style_b, '">Best Box-Cox &#955; via log-likelihood; normal Cpk on transformed data</td>',
                    '</tr>',
                    '</tbody></table>')

                cap_df <- data.frame(
                    Variable    = varlab,
                    Method      = c("Percentile (best fit)", "Box-Cox Transform"),
                    Distribution= c(best$label, paste0("Box-Cox lambda=", round(bc_lambda_used,2))),
                    Cp          = c(cp_pct,  bc_cp),
                    Cpk         = c(cpk_pct, bc_cpk),
                    PPM_OOSpec  = c(pnc_pct, bc_ppm),
                    stringsAsFactors = FALSE)

                if (!is.null(out_env) && is.environment(out_env)) {
                    out_env$cap_table <- cap_df
                }
                if (!is.null(out_env) && is.environment(out_env)) {
                    out_env$dist_rankings <- fits
                    out_env$dist_alpha    <- alpha
                }
            }, error=function(e) {
                cap_tbl_html <<- paste0(
                    '<div style="color:#c0392b;font-size:11px;padding:4px">[Capability error: ',
                    gsub("&","&amp;", gsub("<","&lt;", conditionMessage(e))),
                    '</div>')
            })
        }



        legend_html <- paste0(
            '<div style="display:flex;gap:18px;flex-wrap:wrap;padding:7px 12px;',
            'background:#F8F9FA;border:1px solid #ECF0F1;border-radius:4px;',
            'font-size:11px;margin:4px 0 12px;align-items:center">',
            '<span style="font-weight:bold;color:#555">AD p-value:</span>',
            '<span><span style="display:inline-block;width:11px;height:11px;background:#1E8449;',
            'border-radius:2px;vertical-align:middle;margin-right:4px"></span>',
            '&#8805;0.10 Good</span>',
            '<span><span style="display:inline-block;width:11px;height:11px;background:#D68910;',
            'border-radius:2px;vertical-align:middle;margin-right:4px"></span>',
            '0.05&#8211;0.10 Marginal</span>',
            '<span><span style="display:inline-block;width:11px;height:11px;background:#C0392B;',
            'border-radius:2px;vertical-align:middle;margin-right:4px"></span>',
            '&lt;0.05 Poor</span>',
            '<span style="margin-left:10px;color:#888">',
            sprintf('n=%d &bull; %d distributions tested', n, m_fits),
            '</span></div>')

        hdr_html <- paste0(
            '<div style="background:linear-gradient(135deg,#1A252F 0%,#2C3E50 100%);',
            'color:white;padding:13px 20px;border-radius:6px 6px 0 0;margin-top:22px">',
            '<div style="font-size:15px;font-weight:bold;letter-spacing:0.4px">',
            '&#128200; Data Distribution Advisor &mdash; ', varlab, '</div>',
            '<div style="font-size:11px;opacity:0.72;margin-top:3px">',
            'Best fit: ', best$label, ' &bull; AD p = ',
            if(is.na(best$ad_p)) "N/A" else sprintf("%.4f", best$ad_p),
            ' &bull; AIC = ', sprintf("%.1f", best$aic),
            '</div></div>')

        paste0(
            '<div style="background:white;border:1px solid #d0d0d0;',
            'border-radius:6px;margin-bottom:18px;overflow:hidden">',
            hdr_html,
            '<div style="padding:16px 20px 20px">',
            box_html,
            legend_html,
            spec_toggle_html,
            '<div style="margin-bottom:4px">', gal_div, '</div>',
            gal_dl_btn,
            '<div style="margin-bottom:8px">', ldr_div, '</div>',
            ldr_dl_btn,
            '<h4 style="color:#2C3E50;margin:16px 0 6px;font-size:13px;',
            'border-left:4px solid #3498DB;padding-left:10px">',
            'Full Distribution Rankings</h4>',
            csv_dl_btn,
            tbl_html,
            cap_tbl_html,
            '</div></div>')

    }, error=function(e) {
        paste0('<div style="color:#c0392b;padding:10px;border:1px solid #e74c3c;',
               'border-radius:4px;margin:8px 0;font-size:12px">',
               '<b>[Distribution Advisor error]</b> ',
               gsub("&","&amp;", gsub("<","&lt;", conditionMessage(e))),
               '</div>')
    })
}

# ══════════════════════════════════════════════════════════════════════════════
# SECTION 5 — MAIN FUNCTION
# ══════════════════════════════════════════════════════════════════════════════

processcapability <- function(
    variable    = NULL,  groupvar    = NULL,
    lsl         = NULL,  usl         = NULL,  target      = NULL,
    sigma       = 6.0,   confidence  = 0.95, sigma_method = "POOLED",
    normality   = "YES", ppm         = "YES", cpm         = "YES",
    zbench      = "YES", dpmo        = "YES", yield       = "YES",
    skewness    = "YES", percentiles = "YES", controlimr  = "YES",
    outliers    = "YES", cimean      = "YES", cistd       = "YES",
    cicpk       = "YES",
    boxcox      = "NO",  benchmark   = "YES", reportcard  = "YES",
    generate    = "NO",  samplesize  = 100,   mean        = 30,
    stdev       = 1.5,
    charts      = "YES", histogram   = "YES", normalprob  = "YES",
    capability  = "YES", run         = "YES", boxplot     = "YES",
    summary     = "YES", imr         = "YES", violin      = "YES",
    kde         = "YES", sixpack     = "YES",
    preparedby  = NULL,  reviewedby  = NULL,  company     = NULL,
    logopath    = NULL,
    exportlogs  = "NO",
    datadist    = "NO",  dist_alpha   = 0.05,  dist_include3p = "YES",
    dist_transform = "NO"
) {
    setuplocalization("STATS_PROCESS_CAPABILITY")

    # ── Data Distribution Advisor params ──────────────────────────────────────────
    run_datadist   <- toupper(trimws(as.character(datadist)))      == "YES"
    dist_alpha_val <- tryCatch(as.numeric(dist_alpha), error=function(e) 0.05)
    if (!is.finite(dist_alpha_val)||dist_alpha_val<=0||dist_alpha_val>=1)
        dist_alpha_val <- 0.05
    dist_3p        <- toupper(trimws(as.character(dist_include3p))) == "YES"
    do_transform   <- run_datadist && toupper(trimws(as.character(dist_transform))) == "YES"

    # ── Probability-integral transform helper ────────────────────────────────
    # z = Φ⁻¹(CDF_best(x)): transforms raw data to standard-normal scores.
    # Returns list: z, z_lsl, z_usl (all in transformed space), best_label, success.
    pit_transform <- function(dat, lsl_v, usl_v) {
        result <- list(z=dat, z_lsl=lsl_v, z_usl=usl_v, best_label="", success=FALSE)
        tryCatch({
            fits <- da_fit_all(dat, alpha=dist_alpha_val, include_3p=dist_3p)
            if (length(fits) == 0) return(result)
            best <- fits[[1]]
            p    <- pmax(1e-7, pmin(1-1e-7, sapply(dat, function(x) da_pval(best, x))))
            z    <- qnorm(p)
            z_lsl <- if (!is.null(lsl_v) && is.finite(lsl_v))
                         qnorm(max(1e-7, min(1-1e-7, da_pval(best, lsl_v)))) else lsl_v
            z_usl <- if (!is.null(usl_v) && is.finite(usl_v))
                         qnorm(max(1e-7, min(1-1e-7, da_pval(best, usl_v)))) else usl_v
            list(z=z, z_lsl=z_lsl, z_usl=z_usl, best_label=best$label, success=TRUE)
        }, error=function(e) result)
    }

    procname         <- gtxt("Process Capability Analysis")
    warningsprocname <- gtxt("Process Capability: Warnings")
    omsid            <- "STATSPROCAPABILITY"
    warns            <- Warn(procname = warningsprocname, omsid = omsid)

    # ── Strip quotes from literal-ktype string params ─────────────────────────
    preparedby <- strip_quotes(preparedby)
    reviewedby <- strip_quotes(reviewedby)
    company    <- strip_quotes(company)
    logopath   <- strip_quotes(logopath)

    # ── Parameter validation (stop on invalid input, with a clear message) ────
    # Mirrors the IBM reference Warn() pattern: the offending condition is
    # logged and immediately displayed, then execution halts via
    # warns$warn(msg, dostop = TRUE) — the user must correct the dialog/syntax
    # input before rerunning. This protects every downstream calculation
    # (capability indices, control limits, sigma routing, charts) from being
    # built on a logically inconsistent or out-of-range specification.
    is_num1 <- function(x) !is.null(x) && length(x) == 1L && is.numeric(x) && is.finite(x)

    if (is_num1(lsl) && is_num1(usl) && usl <= lsl)
        warns$warn(gtxtf(paste("Invalid specification limits: USL (%s) must be strictly",
                               "greater than LSL (%s). Check the Lower/Upper Spec Limit fields."),
                         format(usl), format(lsl)), dostop = TRUE)

    if (is_num1(lsl) && is_num1(usl) && is_num1(target) && (target < lsl || target > usl))
        warns$warn(gtxtf(paste("Note: TARGET (%s) is outside the spec limits",
                               "[LSL = %s, USL = %s]. Cpm will not be calculated."),
                         format(target), format(lsl), format(usl)), dostop = FALSE)

    if (!is_num1(sigma) || sigma <= 0)
        warns$warn(gtxtf(paste("Invalid sigma multiplier: SIGMA (%s) must be a positive number",
                               "(commonly 6 for Six Sigma capability, or 3 for a +/-3-sigma",
                               "convention). Check the 'Sigma multiplier' field."),
                         if (is.null(sigma)) "NULL" else format(sigma)), dostop = TRUE)

    if (!is_num1(confidence) || confidence <= 0 || confidence >= 1)
        warns$warn(gtxtf(paste("Invalid confidence level: CONFIDENCE (%s) must be a number",
                               "strictly between 0 and 1 (e.g. 0.95 for 95%%). Check the",
                               "'Confidence level' field."),
                         if (is.null(confidence)) "NULL" else format(confidence)), dostop = TRUE)

    if (to_bool(generate)) {
        if (!is_num1(samplesize) || samplesize <= 0 || samplesize != round(samplesize) || samplesize > 1000000)
            warns$warn(gtxtf(paste("Invalid sample size: SAMPLESIZE (%s) must be a positive whole number",
                                   "between 1 and 1,000,000 when generating simulated data."),
                             if (is.null(samplesize)) "NULL" else format(samplesize)), dostop = TRUE)
        if (!is_num1(stdev) || stdev <= 0 || stdev > 1000000)
            warns$warn(gtxtf(paste("Invalid standard deviation: STDEV (%s) must be a positive",
                                   "number no greater than 1,000,000 when generating simulated data."),
                             if (is.null(stdev)) "NULL" else format(stdev)), dostop = TRUE)
        if (!is_num1(mean))
            warns$warn(gtxtf("Invalid mean: MEAN (%s) must be a number when generating simulated data.",
                             if (is.null(mean)) "NULL" else format(mean)), dostop = TRUE)
    }

    if (!is.null(logopath) && nchar(trimws(logopath)) > 0 &&
        !grepl("\\.(png|jpg|jpeg)$", trimws(logopath), ignore.case = TRUE))
        warns$warn(gtxtf(paste("Invalid logo file: LOGOPATH ('%s') must point to an image",
                               "file with extension .png, .jpg, or .jpeg. Other document types",
                               "(e.g. .docx, .pdf) are not accepted for the report logo."),
                         trimws(logopath)), dostop = TRUE)

    # ── Audit: start ──────────────────────────────────────────────────────────
    do_exportlogs <- to_bool(exportlogs)
    dataset_name <- tryCatch(as.character(spssdata.GetDataSetList()[1]),
                             error = function(e) "Unknown")
    if (do_exportlogs) log_audit_event("ANALYSIS_START", sprintf(
        "dataset=%s; variable=%s; sigma=%s; lsl=%s; usl=%s",
        dataset_name,
        paste(variable, collapse = ","),
        sigma,
        ifelse(is.null(lsl), "NULL", lsl),
        ifelse(is.null(usl), "NULL", usl)
    ))

    # ── Load packages (via OneDrive-backed local mirror) ──────────────────────
    # Downloads spss_cran_mirror_pca.zip from SharePoint on first run, extracts to
    # C:/spss_packages_pca/, then installs. Subsequent runs skip download entirely.
    .spss_ensure_packages(c("nortest","e1071","ggplot2","gridExtra","png","jpeg"))

    # Use requireNamespace (not library()) so that renv-managed installs are
    # found correctly — library() can fail on renv machines even when the
    # package IS installed, because renv's shim intercepts the call and the
    # package may live in a project-specific library that requireNamespace
    # finds via .libPaths() but library() cannot resolve in the same way.
    has_nortest <- requireNamespace("nortest", quietly = TRUE)
    if (!has_nortest)
        warns$warn(gtxt("Package 'nortest' unavailable — Anderson-Darling and Lilliefors tests skipped"))

    has_e1071    <- tryCatch({ library(e1071);     TRUE }, error = function(e) FALSE)
    has_ggplot2  <- tryCatch({
        suppressWarnings(suppressPackageStartupMessages(library(ggplot2))); TRUE
    }, error = function(e) FALSE)
    has_gridExtra <- tryCatch({ library(gridExtra); TRUE }, error = function(e) FALSE)
    has_png      <- tryCatch({ requireNamespace("png",  quietly=TRUE) }, error = function(e) FALSE)
    has_jpeg     <- tryCatch({ requireNamespace("jpeg", quietly=TRUE) }, error = function(e) FALSE)

    # ── Convert flags to logical ───────────────────────────────────────────────
    do_normality    <- to_bool(normality)
    do_ppm          <- to_bool(ppm)
    do_cpm          <- to_bool(cpm)
    do_zbench       <- to_bool(zbench)
    do_dpmo         <- to_bool(dpmo)
    do_yield        <- to_bool(yield)
    do_skewness     <- to_bool(skewness)
    do_percentiles  <- to_bool(percentiles)
    do_controlimr   <- to_bool(controlimr)
    do_outliers     <- to_bool(outliers)
    do_cimean       <- to_bool(cimean)
    do_cistd        <- to_bool(cistd)
    do_cicpk        <- to_bool(cicpk)
    do_boxcox       <- to_bool(boxcox)
    do_benchmark    <- to_bool(benchmark)
    do_reportcard   <- to_bool(reportcard)
    generate_data   <- to_bool(generate)
    create_charts   <- to_bool(charts)
    ch_histogram    <- to_bool(histogram)
    ch_normalprob   <- to_bool(normalprob)
    ch_capability   <- to_bool(capability)
    ch_run          <- to_bool(run)
    ch_boxplot      <- to_bool(boxplot)
    ch_summary      <- to_bool(summary)
    ch_imr          <- to_bool(imr)
    ch_violin       <- to_bool(violin)
    ch_kde          <- to_bool(kde)
    ch_sixpack      <- to_bool(sixpack)

    sigma_mult   <- suppressWarnings(as.numeric(sigma))
    if (is.null(sigma_mult) || !is.finite(sigma_mult) || sigma_mult <= 0 || sigma_mult > 20) {
        warns$warn(gtxtf(
            "Invalid SIGMA value ('%s'): must be a positive number between 1 and 20. Defaulting to 6.",
            as.character(sigma)))
        sigma_mult <- 6.0
    }
    sigma_half   <- sigma_mult / 2.0      # = 3 for 6-sigma
    conf         <- suppressWarnings(as.numeric(confidence))
    if (is.null(conf) || !is.finite(conf) || conf <= 0 || conf >= 1) {
        warns$warn(gtxtf(
            "Invalid CONFIDENCE value ('%s'): must be between 0 and 1. Defaulting to 0.95.",
            as.character(confidence)))
        conf <- 0.95
    }
    sample_size  <- as.integer(samplesize)
    gen_mean_val <- as.numeric(mean)
    gen_std_val  <- as.numeric(stdev)

    # ── Validate spec limits ──────────────────────────────────────────────────
    lsl    <- if (!is.null(lsl)    && length(lsl)    > 0 && !is.na(lsl))
                  as.numeric(lsl)    else NULL
    usl    <- if (!is.null(usl)    && length(usl)    > 0 && !is.na(usl))
                  as.numeric(usl)    else NULL
    target <- if (!is.null(target) && length(target) > 0 && !is.na(target))
                  as.numeric(target) else NULL

    has_specs <- !is.null(lsl) || !is.null(usl)
    # Spec limits are OPTIONAL — analysis continues without them (descriptive + control charts)
    if (!has_specs)
        warns$warn(gtxt(paste(
            "No specification limits provided.",
            "Capability indices and PPM will not be calculated.",
            "Descriptive statistics and control charts will still be produced."
        )))

    if (!is.null(lsl) && !is.null(usl) && lsl >= usl)
        warns$warn(gtxt("LSL must be strictly less than USL."), dostop = TRUE)

    if (!is.null(target)) {
        if (!is.null(lsl) && target < lsl)
            warns$warn(gtxt("Warning: Target is below LSL."))
        if (!is.null(usl) && target > usl)
            warns$warn(gtxt("Warning: Target is above USL."))
    }

    # ── Open SPSS procedure ───────────────────────────────────────────────────
    StartProcedure(procname, omsid)

    tryCatch({

        # ════════════════════════════════════════════════════════════════════
        # 5.1  DATA ACQUISITION — supports multiple variables
        # ════════════════════════════════════════════════════════════════════

        var_list <- character(0)

        if (generate_data) {
            set.seed(123)
            synth    <- rnorm(sample_size, mean = gen_mean_val, sd = gen_std_val)
            var_list <- "__GENERATED__"
            all_data   <- list("__GENERATED__" = synth)
            all_groups <- list("__GENERATED__" = NULL)
            spsspivottable.Display(
                data.frame(
                    Parameter = c("Sample Size", "Mean", "Std Dev", "Distribution"),
                    Value     = c(sample_size, gen_mean_val, gen_std_val, "Normal (seed=123)"),
                    stringsAsFactors = FALSE),
                title   = gtxt("Generated Data Information"),
                caption = gtxt("Synthetic data — not from active dataset"))
        } else {
            if (is.null(variable) || (is.character(variable) && all(nchar(trimws(variable)) == 0)))
                warns$warn(gtxt("No process variable(s) specified."), dostop = TRUE)

            # variable may be a character vector (multi-variable)
            var_list <- as.character(unlist(variable))
            var_list <- var_list[nchar(trimws(var_list)) > 0]

            all_data   <- list()
            all_groups <- list()

            has_grp   <- !is.null(groupvar) && nchar(trimws(as.character(groupvar))) > 0
            # ── KEY FIX: GetDataFromSPSS can only be called ONCE per procedure.
            #    Read ALL needed columns in a single call with factorMode="labels"
            #    (works for both numeric process variables AND string grouping variables).
            read_cols <- unique(c(var_list,
                                  if (has_grp) as.character(groupvar) else character(0)))

            raw_all <- tryCatch(
                spssdata.GetDataFromSPSS(variables        = read_cols,
                                         missingValueToNA = TRUE,
                                         factorMode       = "labels"),
                error = function(e) {
                    warns$warn(gtxtf("Cannot read dataset: %s", e$message), dostop = TRUE)
                    NULL
                })

            if (is.null(raw_all) || nrow(raw_all) == 0)
                warns$warn(gtxt("Dataset has no valid data."), dostop = TRUE)

            for (v in var_list) {
                if (!v %in% names(raw_all)) {
                    warns$warn(gtxtf("Variable '%s' not found in dataset — skipped.", v))
                    next
                }
                valid_mask <- !is.na(raw_all[[v]])
                dat <- suppressWarnings(as.numeric(as.character(raw_all[[v]][valid_mask])))
                dat <- dat[!is.na(dat)]

                if (length(dat) < 2) {
                    warns$warn(gtxtf("Variable '%s' has fewer than 2 valid values — skipped.", v))
                    next
                }

                grp <- if (has_grp && groupvar %in% names(raw_all))
                           as.character(raw_all[[groupvar]][valid_mask]) else NULL

                all_data[[v]]   <- dat
                all_groups[[v]] <- grp
            }

            if (length(all_data) == 0)
                warns$warn(gtxt("No valid variables to analyze."), dostop = TRUE)

            var_list <- names(all_data)
        }

        n_vars    <- length(var_list)
        multi_var <- n_vars > 1

        # ════════════════════════════════════════════════════════════════════
        # 5.2  PER-VARIABLE ANALYSIS
        # ════════════════════════════════════════════════════════════════════

        results <- list()

        for (v in var_list) {
            data   <- all_data[[v]]
            groups <- all_groups[[v]]
            n      <- length(data)
            varlab <- if (v == "__GENERATED__") "Generated Data" else {
                lbl <- tryCatch({
                    nms <- spssdictionary.GetVariableNames()
                    idx <- which(toupper(nms) == toupper(v))
                    if (length(idx) > 0L) {
                        lb <- spssdictionary.GetVariableLabel(idx[1L] - 1L)
                        if (!is.null(lb) && nchar(trimws(lb)) > 0L) lb else v
                    } else v
                }, error = function(e) v)
                lbl
            }

            # ── Variable sanity checks (warnings only — analysis still runs) ──
            if (v != "__GENERATED__") {

                # Check 1: SPSS date/time format variable
                # If the user accidentally selected a date/datetime field, the
                # internal numeric values (seconds from epoch ~1.4e10) will make
                # the capability indices numerically valid but physically meaningless.
                tryCatch({
                    all_spss_names <- spssdictionary.GetVariableNames()
                    v_idx <- which(toupper(all_spss_names) == toupper(v))
                    if (length(v_idx) > 0) {
                        v_fmt <- spssdictionary.GetVariableFormatType(v_idx[1] - 1L)
                        date_time_pattern <- paste0("^(DATE|ADATE|EDATE|JDATE|SDATE|",
                                                    "QYR|MOYR|WKYR|WKDAY|MONTH|",
                                                    "TIME|DATETIME|DTIME|MTIME|YMDHMS)")
                        if (grepl(date_time_pattern, v_fmt, ignore.case = TRUE)) {
                            warns$warn(gtxtf(paste0(
                                "Variable '%s' has an SPSS date/time format (%s). ",
                                "Capability analysis on date/time values is unlikely ",
                                "to be meaningful — please verify you selected the ",
                                "correct process measurement variable."), v, v_fmt))
                        }
                    }
                }, error = function(e) NULL)  # silently skip if dictionary API unavailable

                # Check 2: Strictly monotonic raw-order data (likely a time index or sequence)
                # Process measurements collected over time are random around a mean;
                # a strictly increasing/decreasing sequence across all rows almost
                # always means the variable is an observation index, timestamp, or
                # row number — not a real process measurement.
                if (n >= 5) {
                    raw_diffs <- diff(data)   # original dataset row order
                    if (all(raw_diffs > 0)) {
                        warns$warn(gtxtf(paste0(
                            "Variable '%s': all values are strictly increasing in ",
                            "dataset row order. This may indicate a time index, ",
                            "sequence number, or running total rather than a process ",
                            "measurement. Capability indices will be calculated but ",
                            "results should be interpreted with caution."), varlab))
                    } else if (all(raw_diffs < 0)) {
                        warns$warn(gtxtf(paste0(
                            "Variable '%s': all values are strictly decreasing in ",
                            "dataset row order. This may indicate a reverse-ordered ",
                            "time index or sequence. Capability indices will be ",
                            "calculated but results should be interpreted with caution."),
                            varlab))
                    }
                }
            }

            # ── Descriptive stats ─────────────────────────────────────────
            xbar    <- mean(data)
            s_total <- sd(data)              # overall / total SD
            xmin    <- min(data)
            xmax    <- max(data)
            xmed    <- median(data)
            xrange  <- xmax - xmin
            xse     <- s_total / sqrt(n)

            # ── Within-subgroup SD (for Cp/Cpk) ──────────────────────────
            # Standard SPC convention uses within-subgroup variation for Cp/Cpk,
            # overall variation for Pp/Ppk.
            # Without subgroups, both SDs are identical.
            # ── Sigma-method dispatcher ──────────────────────────────────
            # Routes the within-subgroup sigma estimate to one of four
            # standard SPC methods, selectable via the SIGMAMETHOD keyword
            # (mirrors the standard "Estimate of standard deviation" options seen in SPC software):
            #   POOLED -> sqrt[ Σ(nᵢ-1)sᵢ² / Σ(nᵢ-1) ]   (statistically efficient)
            #   RBAR   -> Rbar / d2(nbar)                  (Xbar-R default)
            #   SBAR   -> Sbar / c4(nbar)                  (Xbar-S default)
            #   MRBAR  -> MRbar / d2(2) = MRbar / 1.128    (individuals / I-MR data)
            sm <- toupper(trimws(if (is.null(sigma_method)) "POOLED" else sigma_method))
            if (!sm %in% c("POOLED", "RBAR", "SBAR", "MRBAR")) sm <- "POOLED"

            # ── Probability-integral transform (if enabled) ─────────────────────
            orig_data <- data; orig_lsl <- lsl; orig_usl <- usl  # preserve before any transform
            transform_ok <- FALSE
            transform_label <- ""
            if (do_transform && length(data) >= 4) {
                pit <- pit_transform(data, lsl, usl)
                if (pit$success) {
                    transform_ok   <- TRUE
                    data   <- pit$z
                    lsl    <- pit$z_lsl
                    usl    <- pit$z_usl
                    transform_label <- paste0(" [Transform: ", pit$best_label, "]")
                } else {
                    warns$warn(gtxtf(
                        "Transform to fit failed for '%s' — using original data.",
                        varlab))
                }
            }

            # ── Recompute ALL descriptive stats on z-score scale when transform active ──
            # xbar/s_total were computed from original data above; after PIT they
            # MUST be recomputed so capability indices, PPM, Z-bench, I-MR limits,
            # and all SPSS tables are 100%% on the same scale as the transformed data.
            if (transform_ok) {
                xbar    <- mean(data)
                s_total <- sd(data)
                xmin    <- min(data)
                xmax    <- max(data)
                xmed    <- median(data)
                xrange  <- xmax - xmin
                xse     <- s_total / sqrt(n)
            }

            has_groups <- !is.null(groups) && length(unique(groups[!is.na(groups)])) > 1
            sm_note    <- NULL   # set when a fallback substitution occurs

            sm_actual <- sm   # tracks the method actually used (may differ from requested)

            if (sm == "MRBAR") {
                s_within <- compute_mrbar_sd(data)
                if (!is.finite(s_within) || s_within <= 0) {
                    s_within  <- s_total
                    sm_actual <- "OVERALL"
                    sm_note   <- "MRbar/d2(2) undefined (n<2 or constant data) — overall SD used instead"
                    warns$warn(gtxtf("'%s': MRbar/d2(2) could not be computed — falling back to overall SD for Cp/Cpk.", varlab))
                }
            } else if (sm == "RBAR") {
                if (has_groups) {
                    s_within <- compute_rbar_sd(data, groups)
                    if (!is.finite(s_within) || s_within <= 0) {
                        s_within  <- compute_within_sd(data, groups)
                        sm_actual <- "POOLED"
                        sm_note   <- "Rbar/d2 could not be computed (all subgroups have n=1 or constant ranges) — pooled SD used instead"
                        warns$warn(gtxtf("'%s': Rbar/d2 undefined — falling back to pooled within-SD.", varlab))
                    }
                } else {
                    s_within  <- compute_mrbar_sd(data)
                    sm_actual <- "MRBAR"
                    sm_note   <- "Rbar/d2 is undefined without a groupvar — routed to MRbar/d2(2) (standard individuals estimator)"
                    if (!is.finite(s_within) || s_within <= 0) {
                        s_within  <- s_total
                        sm_actual <- "OVERALL"
                        sm_note   <- "Rbar/d2 undefined (no groupvar) and MRbar/d2(2) also undefined — overall SD used instead"
                        warns$warn(gtxtf("'%s': Rbar/d2 and MRbar/d2(2) both undefined — falling back to overall SD.", varlab))
                    }
                }
            } else if (sm == "SBAR") {
                if (has_groups) {
                    s_within <- compute_sbar_sd(data, groups)
                    if (!is.finite(s_within) || s_within <= 0) {
                        s_within  <- compute_within_sd(data, groups)
                        sm_actual <- "POOLED"
                        sm_note   <- "Sbar/c4 could not be computed (all subgroups have n=1 or constant SDs) — pooled SD used instead"
                        warns$warn(gtxtf("'%s': Sbar/c4 undefined — falling back to pooled within-SD.", varlab))
                    }
                } else {
                    s_within  <- compute_mrbar_sd(data)
                    sm_actual <- "MRBAR"
                    sm_note   <- "Sbar/c4 is undefined without a groupvar — routed to MRbar/d2(2) (standard individuals estimator)"
                    if (!is.finite(s_within) || s_within <= 0) {
                        s_within  <- s_total
                        sm_actual <- "OVERALL"
                        sm_note   <- "Sbar/c4 undefined (no groupvar) and MRbar/d2(2) also undefined — overall SD used instead"
                        warns$warn(gtxtf("'%s': Sbar/c4 and MRbar/d2(2) both undefined — falling back to overall SD.", varlab))
                    }
                }
            } else {  # POOLED (default)
                if (has_groups) {
                    s_within <- compute_within_sd(data, groups)
                    # compute_within_sd returns sd(data) when all subgroups have n=1 (df_sum=0)
                    # — detect this case and warn so user knows pooled within-SD is actually overall
                    all_singleton <- all(table(groups[!is.na(groups)]) == 1)
                    if (all_singleton) {
                        sm_actual <- "OVERALL"
                        sm_note   <- "All subgroups have n=1 — pooled within-SD is undefined; overall SD used for Cp/Cpk"
                        warns$warn(gtxtf(paste0("'%s': All subgroup sizes are 1 — pooled within-SD cannot be computed. ",
                                                "Consider using MRBAR sigma method for individual data."), varlab))
                    }
                } else {
                    # Individual data: pooled SD requires subgroups (undefined without them).
                    # AIAG SPC manual / Minitab standard: use MRbar/d2(2) for Cp/Cpk on
                    # individual data — this is what makes Cpk differ from Ppk on ungrouped data.
                    s_within  <- compute_mrbar_sd(data)
                    sm_actual <- "MRBAR"
                    sm_note   <- "No groupvar: pooled SD undefined for individual data — MRbar/d2(2) applied for Cp/Cpk (AIAG/Minitab standard). Pp/Ppk use overall SD."
                    if (!is.finite(s_within) || s_within <= 0) {
                        s_within  <- s_total
                        sm_actual <- "OVERALL"
                        sm_note   <- "No groupvar and MRbar/d2(2) also undefined — overall SD used for Cp/Cpk"
                        warns$warn(gtxtf("'%s': MRbar/d2(2) undefined for individual data — falling back to overall SD.", varlab))
                    }
                }
            }

            # sm_label reflects the METHOD ACTUALLY USED (not just the one requested)
            sm_label <- switch(sm_actual,
                POOLED  = "Pooled Standard Deviation",
                RBAR    = "Rbar / d2  (Xbar-R)",
                SBAR    = "Sbar / c4  (Xbar-S)",
                MRBAR   = "MRbar / d2(2)  (Individuals / I-MR)",
                OVERALL = "Overall SD  (fallback — within-SD undefined)",
                paste0("Unknown (", sm_actual, ")")
            )
            # Append fallback note if a substitution occurred
            sm_display <- if (!is.null(sm_note) && sm != sm_actual)
                paste0(sm_label, "  [NOTE: ", sm_note, "]")
            else if (!is.null(sm_note) && sm == "POOLED" && sm_actual == "MRBAR")
                paste0(sm_label, "  [NOTE: ", sm_note, "]")
            else
                sm_label

            transform_note <- if (transform_ok)
                paste0("  |  ⚠ All statistics below are on the z-score scale",
                       " (", pit$best_label, " PIT transform applied)")
            else ""

            spsspivottable.Display(
                data.frame(
                    Statistic = c("n", "Mean (x̄)", "Std Dev — Overall (s)",
                                  "Std Dev — Within-Subgroup",
                                  "Std Error (SE)", "Min", "Max", "Range", "Median"),
                    Value = c(n, round(xbar, 6), round(s_total, 6),
                              round(s_within, 6), round(xse, 6),
                              round(xmin, 6), round(xmax, 6), round(xrange, 6),
                              round(xmed, 6)),
                    stringsAsFactors = FALSE),
                title   = gtxtf("Descriptive Statistics — %s%s", varlab, transform_label),
                caption = paste0(gtxtf("Sigma method: %s", sm_display), transform_note))

            # ── Normality tests ───────────────────────────────────────────
            ad_stat <- NA_real_; ad_p <- NA_real_
            sw_stat <- NA_real_; sw_p <- NA_real_
            is_normal <- TRUE  # assume normal unless test says otherwise

            if (do_normality && n >= 3) {
                fmt_p <- function(p) if (is.na(p)) "N/A"
                                     else if (p < 0.001) "< 0.001"
                                     else formatC(p, digits = 4, format = "f")
                interp <- function(p) if (is.na(p)) "N/A"
                                      else if (p > 0.05) "Fail to reject H₀ — Normal"
                                      else "Reject H₀ — Non-Normal"

                # nortest::ad.test requires n >= 8 finite values.
                # After a PIT transform, data may contain NaN/Inf z-scores;
                # filter them first so the internal n check inside ad.test
                # sees the correct count and doesn't throw "sample size must
                # be greater than 7" which would be silently swallowed here.
                ad_data <- data[is.finite(data)]
                if (has_nortest && length(ad_data) >= 8)
                    tryCatch({
                        t <- nortest::ad.test(ad_data)
                        ad_stat <- t$statistic[[1]]; ad_p <- t$p.value
                    }, error = function(e) NULL)

                # shapiro.test accepts 3–5000 values and handles NAs via
                # complete.cases internally, but pass finite-only to be safe.
                sw_data <- data[is.finite(data)]
                if (length(sw_data) >= 3 && length(sw_data) <= 5000)
                    tryCatch({
                        t <- shapiro.test(sw_data)
                        sw_stat <- t$statistic[[1]]; sw_p <- t$p.value
                    }, error = function(e) NULL)

                # Jarque-Bera (tests joint skewness + excess kurtosis)
                jb_stat <- NA_real_; jb_p <- NA_real_
                tryCatch({
                    jb_r <- da_jb_test(data)
                    jb_stat <- jb_r$stat; jb_p <- jb_r$p
                }, error = function(e) NULL)

                # Kolmogorov-Smirnov with Lilliefors correction
                # Use nortest::lillie.test when available (proper correction for
                # estimated parameters). Fall back to standard KS on scaled data
                # (p-value is liberal) if nortest is absent.
                ks_stat <- NA_real_; ks_p <- NA_real_; ks_lillie <- FALSE
                ks_data <- data[is.finite(data)]
                tryCatch({
                    if (has_nortest && length(ks_data) >= 5) {
                        # lillie.test requires n >= 5
                        ks_r    <- nortest::lillie.test(ks_data)
                        ks_stat <- ks_r$statistic[[1]]; ks_p <- ks_r$p.value
                        ks_lillie <- TRUE
                    } else if (length(ks_data) >= 2) {
                        ks_r    <- ks.test(scale(ks_data), "pnorm")
                        ks_stat <- ks_r$statistic[[1]]; ks_p <- ks_r$p.value
                    }
                }, error = function(e) NULL)

                # Normality decision: prefer AD (most powerful), fall back to
                # SW when AD is unavailable, assume normal if no test ran.
                is_normal <- if (!is.na(ad_p)) ad_p > 0.05
                             else if (!is.na(sw_p)) sw_p > 0.05
                             else TRUE

                spsspivottable.Display(
                    data.frame(
                        Test = c(
                            "Anderson-Darling",
                            if (n <= 5000) "Shapiro-Wilk" else "Shapiro-Wilk (n>5000, skipped)",
                            "Jarque-Bera",
                            if (ks_lillie) "Kolmogorov-Smirnov (Lilliefors)" else "Kolmogorov-Smirnov (std, params estimated)"),
                        Statistic = c(
                            if (!is.na(ad_stat)) formatC(ad_stat,4,format="f") else "N/A",
                            if (!is.na(sw_stat)) formatC(sw_stat,4,format="f") else "N/A",
                            if (!is.na(jb_stat)) formatC(jb_stat,4,format="f") else "N/A",
                            if (!is.na(ks_stat)) formatC(ks_stat,4,format="f") else "N/A"),
                        `P-Value` = c(fmt_p(ad_p), fmt_p(sw_p), fmt_p(jb_p), fmt_p(ks_p)),
                        Result    = c(interp(ad_p), interp(sw_p), interp(jb_p), interp(ks_p)),
                        stringsAsFactors = FALSE, check.names = FALSE),
                    title   = gtxtf("Normality Tests — %s", varlab),
                    caption = paste0(
                        "H₀: Normal distribution  |  α = 0.05  |  SW: Shapiro-Wilk (Minitab uses Ryan-Joiner — different test, different values expected)  |  ",
                        "JB: Bera-Jarque (1981), chi-sq(2); biased moments; unreliable n<50  |  ",
                        if (ks_lillie) "KS: Lilliefors correction applied (nortest)" else "KS: std test, p-value liberal (estimated params, nortest not installed)",
                        if (transform_ok) paste0(
                            "\n⚠ Tests run on z-score transformed data — normality",
                            " is expected by construction when transform succeeded") else ""))
            }

            # ── Capability indices ────────────────────────────────────────
            # VALIDATED FORMULAS (standard SPC-equivalent):
            #   Cp  = (USL-LSL) / (sigma_mult * s_within)   [6s for 6-sigma]
            #   Cpl = (x̄ - LSL) / (sigma_half * s_within)   [3s for 6-sigma]
            #   Cpu = (USL - x̄) / (sigma_half * s_within)   [3s for 6-sigma]
            #   Cpk = min(Cpl, Cpu)
            #   Pp  = (USL-LSL) / (sigma_mult * s_total)     [overall variation]
            #   Ppk = min(Ppl, Ppu) with s_total
            #   Cpm = (USL-LSL) / (6.0 * sqrt(s²+(x̄-T)²))  [Taguchi / ISO 22514: always 6σ regardless of sigma_mult]

            cp  <- NA_real_; cpk <- NA_real_
            cpl <- NA_real_; cpu <- NA_real_
            pp  <- NA_real_; ppk <- NA_real_; ppl <- NA_real_; ppu <- NA_real_
            cpm_val <- NA_real_
            n_below <- 0L; n_above <- 0L
            ppm_below <- NA_real_; ppm_above <- NA_real_; ppm_total <- NA_real_
            pct_out <- 0

            # ── Zero-SD guard ─────────────────────────────────────────────────
            # Constant data (all values identical) produces s_total=0 and/or s_within=0,
            # which would give Inf capability indices. Detect and warn; skip index calc.
            if (s_total <= 0 || !is.finite(s_total)) {
                warns$warn(gtxtf(
                    "Variable '%s': overall standard deviation is zero (all values identical). ",
                    "Capability indices cannot be computed.", varlab))
                has_specs <- FALSE   # skip index block below
            } else if (!is.finite(s_within) || s_within <= 0) {
                warns$warn(gtxtf(
                    "Variable '%s': within-subgroup SD is zero or undefined. ",
                    "Cp/Cpk will be calculated using overall SD; Pp/Ppk unaffected.", varlab))
                s_within <- s_total
            }

            if (has_specs) {
                if (!is.null(lsl) && !is.null(usl)) {
                    cp      <- (usl - lsl) / (sigma_mult * s_within)
                    cpl     <- (xbar - lsl) / (sigma_half * s_within)
                    cpu     <- (usl - xbar) / (sigma_half * s_within)
                    cpk     <- min(cpl, cpu)
                    pp      <- (usl - lsl) / (sigma_mult * s_total)
                    ppl     <- (xbar - lsl) / (sigma_half * s_total)
                    ppu     <- (usl - xbar) / (sigma_half * s_total)
                    ppk     <- min(ppl, ppu)
                    if (do_cpm && !is.null(target))
                        # Cpm (Taguchi / Chan et al. 1988 / ISO 22514):
                        # denominator is ALWAYS 6*sigma_T regardless of sigma_mult chosen.
                        # sigma_T = sqrt(s^2 + (xbar-target)^2)
                        cpm_val <- (usl - lsl) /
                                   (6.0 * sqrt(s_total^2 + (xbar - target)^2))
                    n_below   <- sum(data < lsl); n_above <- sum(data > usl)
                } else if (!is.null(lsl)) {
                    cpl <- (xbar - lsl) / (sigma_half * s_within)
                    cpk <- cpl
                    ppl <- (xbar - lsl) / (sigma_half * s_total); ppk <- ppl
                    n_below <- sum(data < lsl)
                } else {
                    cpu <- (usl - xbar) / (sigma_half * s_within)
                    cpk <- cpu
                    ppu <- (usl - xbar) / (sigma_half * s_total); ppk <- ppu
                    n_above <- sum(data > usl)
                }

                pct_out <- 100 * (n_below + n_above) / n

                if (do_ppm) {
                    if (!is.null(lsl)) ppm_below <- pnorm((lsl - xbar) / s_total) * 1e6
                    if (!is.null(usl)) ppm_above <- (1 - pnorm((usl - xbar) / s_total)) * 1e6
                    ppm_total <- sum(c(ppm_below, ppm_above), na.rm = TRUE)
                }

                # ── Capability indices table ──────────────────────────────
                idx_rows <- list()
                add_idx  <- function(nm, w, o) {
                    if (!is.na(w) || !is.na(o))
                        idx_rows[[length(idx_rows)+1]] <<-
                            data.frame(Index    = nm,
                                       Within   = if (!is.na(w)) formatC(w,4,format="f") else "N/A",
                                       Overall  = if (!is.na(o)) formatC(o,4,format="f") else "N/A",
                                       Grade    = grade_cap(if (!is.na(w)) w else o)$grade,
                                       stringsAsFactors = FALSE)
                }
                add_idx("Cp  (Potential)",         cp,      pp)
                add_idx("Cpk / Ppk (Actual)",      cpk,     ppk)
                add_idx("Cpl (Lower Index)",        cpl,     ppl)
                add_idx("Cpu (Upper Index)",        cpu,     ppu)
                add_idx("Cpm (Taguchi — if target)", cpm_val, NA_real_)

                if (length(idx_rows) > 0) {
                    tbl <- do.call(rbind, idx_rows); row.names(tbl) <- NULL
                    spsspivottable.Display(tbl,
                        title   = gtxtf("Process Capability Indices — %s%s", varlab, transform_label),
                        caption = paste0(
                            gtxtf(
                                "%g-sigma  |  Within SD=%.4f (Cp/Cpk)  |  Overall SD=%.4f (Pp/Ppk)\nGrade scale (Cpk):  ≥ 1.67 = Excellent  |  ≥ 1.33 = Capable  |  ≥ 1.00 = Marginal  |  < 1.00 = Not Capable",
                                sigma_mult, s_within, s_total),
                            if (transform_ok) paste0(
                                "\n⚠ Indices on z-score scale via ", pit$best_label,
                                " PIT transform — spec limits transformed equivalently") else ""))
                }
            }

            # ── Box-Cox non-normal capability (deep integration) ──────────
            # Standard Box-Cox capability-analysis convention (consistent
            # with AIAG/ASQ guidance for non-normal data): transform the raw
            # data AND the spec limits with the *same* fitted lambda and
            # shift, then apply the ordinary Cp/Cpk/Pp/Ppk formulas to the
            # transformed values. Because a monotonic Box-Cox transform has
            # no natural subgroup-level analogue, the transformed-scale
            # indices are computed from the transformed overall SD only —
            # i.e. they are Pp/Ppk-equivalent (overall-variation) indices on
            # the normalised scale, which is what Box-Cox capability reports
            # in reference texts present. This is stated explicitly in the
            # table caption below to avoid any confusion with within-subgroup
            # Cp/Cpk on the original scale.
            bc_lam <- NA_real_; bc_shift <- 0
            cp_bc  <- NA_real_; cpk_bc <- NA_real_
            pp_bc  <- NA_real_; ppk_bc <- NA_real_
            sw_p_bc <- NA_real_

            if (do_boxcox && !isTRUE(transform_ok) && has_specs) {
                tryCatch({
                    data_pos <- data
                    bc_shift <- 0
                    if (any(data_pos <= 0)) {
                        bc_shift  <- abs(min(data_pos)) + 0.001
                        data_pos  <- data_pos + bc_shift
                    }
                    bc_lam  <- boxcox_lambda(data_pos)
                    data_bc <- bc_transform(data_pos, bc_lam, 0)

                    lsl_bc <- if (!is.null(lsl)) bc_transform(lsl + bc_shift, bc_lam, 0) else NULL
                    usl_bc <- if (!is.null(usl)) bc_transform(usl + bc_shift, bc_lam, 0) else NULL

                    xbar_bc <- mean(data_bc, na.rm = TRUE)
                    s_bc    <- sd(data_bc, na.rm = TRUE)

                    # Re-check normality on the transformed scale (Shapiro-Wilk,
                    # capped at 5000 obs per the standard test's sample limit)
                    # so users can see whether the transform actually helped.
                    sw_p_bc <- tryCatch({
                        dvec <- data_bc[is.finite(data_bc)]
                        shapiro.test(if (length(dvec) > 5000) sample(dvec, 5000) else dvec)$p.value
                    }, error = function(e) NA_real_)

                    if (!is.null(lsl_bc) && !is.null(usl_bc) && is.finite(lsl_bc) && is.finite(usl_bc)) {
                        cp_bc  <- (usl_bc - lsl_bc) / (sigma_mult * s_bc)
                        cpk_bc <- min((xbar_bc - lsl_bc) / (sigma_half * s_bc),
                                      (usl_bc - xbar_bc) / (sigma_half * s_bc))
                        # Pp/Ppk-equivalent — identical formula on the
                        # transformed scale, since both use overall SD here.
                        pp_bc  <- cp_bc
                        ppk_bc <- cpk_bc
                    } else if (!is.null(lsl_bc) && is.finite(lsl_bc)) {
                        cp_bc <- cpk_bc <- pp_bc <- ppk_bc <-
                            (xbar_bc - lsl_bc) / (sigma_half * s_bc)
                    } else if (!is.null(usl_bc) && is.finite(usl_bc)) {
                        cp_bc <- cpk_bc <- pp_bc <- ppk_bc <-
                            (usl_bc - xbar_bc) / (sigma_half * s_bc)
                    }

                    spsspivottable.Display(
                        data.frame(
                            Item  = c("Box-Cox Lambda (λ)", "Data Shift Applied",
                                      "Transformed Mean", "Transformed Std Dev",
                                      "Shapiro-Wilk p (transformed)",
                                      "Cp/Pp — equivalent (transformed)",
                                      "Cpk/Ppk — equivalent (transformed)"),
                            Value = c(formatC(bc_lam, 4, format="f"),
                                      if (bc_shift > 0) formatC(bc_shift, 4, format="f") else "None",
                                      formatC(xbar_bc, 4, format="f"),
                                      formatC(s_bc, 4, format="f"),
                                      if (!is.na(sw_p_bc)) formatC(sw_p_bc, 4, format="f") else "N/A",
                                      if (!is.na(cp_bc))  formatC(cp_bc,  4, format="f") else "N/A",
                                      if (!is.na(cpk_bc)) formatC(cpk_bc, 4, format="f") else "N/A"),
                            stringsAsFactors = FALSE),
                        title   = gtxtf("Box-Cox Non-Normal Capability — %s", varlab),
                        caption = "Pp/Ppk-equivalent, transformed scale")

                    # ── Before vs. after comparison ────────────────────────
                    # Side-by-side view of capability and normality on the
                    # original scale vs. the Box-Cox-transformed scale, so
                    # the user can judge whether transforming actually
                    # changed the capability assessment or just the normality
                    # diagnostic.
                    cmp_metric <- c("Pp  (overall potential)",
                                    "Ppk (overall actual)",
                                    "Normality test p-value",
                                    "Assessment")
                    cmp_before <- c(if (!is.na(pp))  formatC(pp,  4, format="f") else "N/A",
                                    if (!is.na(ppk)) formatC(ppk, 4, format="f") else "N/A",
                                    if (!is.na(sw_p)) formatC(sw_p, 4, format="f")
                                    else if (!is.na(ad_p)) formatC(ad_p, 4, format="f")
                                    else "N/A",
                                    if (is_normal) "Approximately normal" else "Non-normal")
                    cmp_after  <- c(if (!is.na(pp_bc))  formatC(pp_bc,  4, format="f") else "N/A",
                                    if (!is.na(ppk_bc)) formatC(ppk_bc, 4, format="f") else "N/A",
                                    if (!is.na(sw_p_bc)) formatC(sw_p_bc, 4, format="f") else "N/A",
                                    if (!is.na(sw_p_bc) && sw_p_bc > 0.05) "Approximately normal"
                                    else if (!is.na(sw_p_bc)) "Still non-normal"
                                    else "N/A")
                    spsspivottable.Display(
                        data.frame(Metric     = cmp_metric,
                                   Original    = cmp_before,
                                   Transformed = cmp_after,
                                   stringsAsFactors = FALSE),
                        title   = gtxtf("Normal vs. Box-Cox-Transformed Comparison — %s", varlab))
                }, error = function(e)
                    warns$warn(gtxtf("Box-Cox transformation failed: %s", e$message)))
            }

            # ── Performance metrics ───────────────────────────────────────
            if (has_specs) {
                perf_nm <- c(); perf_vl <- c()
                addp <- function(nm, v) { perf_nm <<- c(perf_nm, nm); perf_vl <<- c(perf_vl, v) }
                if (!is.null(lsl)) addp("Observed — Below LSL",
                                        sprintf("%d (%.4f%%)", n_below, 100*n_below/n))
                if (!is.null(usl)) addp("Observed — Above USL",
                                        sprintf("%d (%.4f%%)", n_above, 100*n_above/n))
                addp("Observed — Total Out of Spec",
                     sprintf("%d (%.4f%%)", n_below+n_above, pct_out))
                if (!is.na(ppm_total)) {
                    if (!is.null(lsl) && !is.na(ppm_below))
                        addp("Expected PPM — Below LSL", formatC(ppm_below, 2, format="f"))
                    if (!is.null(usl) && !is.na(ppm_above))
                        addp("Expected PPM — Above USL", formatC(ppm_above, 2, format="f"))
                    addp("Expected PPM — Total",         formatC(ppm_total, 2, format="f"))
                    if (do_dpmo)  addp("Expected DPMO  (= PPM when DPO=1)",
                                       formatC(ppm_total, 2, format="f"))
                    if (do_yield) addp("Expected Yield",
                                       sprintf("%.6f%%", 100*(1 - ppm_total/1e6)))
                }
                spsspivottable.Display(
                    data.frame(Metric=perf_nm, Value=perf_vl,
                               stringsAsFactors=FALSE, row.names=NULL),
                    title   = gtxtf("Performance Metrics — %s%s", varlab, transform_label),
                    caption = paste0(
                        "Observed: actual counts  |  Expected: from fitted normal distribution",
                        if (transform_ok) "\n⚠ Expected PPM/yield computed on z-score scale" else ""))
            }

            # ── Z-Bench, Benchmark Sigma, 1.5σ Six Sigma table ───────────
            z_bench <- NA_real_

            if (has_specs && !is.na(ppm_total) && ppm_total > 0 && ppm_total < 1e6) {
                z_bench <- qnorm(1 - ppm_total / 1e6)

                if (do_zbench) {
                    qual <- if      (z_bench >= 6) "World Class  (≥ 6σ)"
                            else if (z_bench >= 5) "Excellent    (≥ 5σ)"
                            else if (z_bench >= 4) "Good         (≥ 4σ)"
                            else if (z_bench >= 3) "Average      (≥ 3σ)"
                            else                    "Below Average  (< 3σ)"
                    spsspivottable.Display(
                        data.frame(Metric = c("Z-Bench — Overall (Long-Term)", "Sigma Level — Overall"),
                                   Value  = c(formatC(z_bench, 4, format="f"), qual),
                                   stringsAsFactors = FALSE),
                        title   = gtxtf("Z-Bench / Sigma Level — %s", varlab),
                        caption = "Overall-SD basis (Pp/Ppk)")
                }

                if (do_benchmark) {
                    # Six Sigma convention: real processes drift ±1.5σ over time.
                    # Long-term (from data): Z_LT = Z_bench
                    # Short-term (if process were centred): Z_ST = Z_LT + 1.5
                    z_lt     <- z_bench
                    z_st     <- z_lt + 1.5
                    dpmo_lt  <- ppm_total
                    # Derive DPMO short-term DIRECTLY from Z_st so the displayed
                    # Sigma-Level and DPMO numbers are mutually consistent
                    # (i.e. qnorm(1 - DPMO_st/1e6) == Z_st exactly), for both
                    # one-sided and two-sided specs alike.
                    dpmo_st  <- pnorm(-z_st) * 1e6
                    yield_lt <- 100 * (1 - dpmo_lt / 1e6)
                    yield_st <- 100 * (1 - dpmo_st / 1e6)

                    spsspivottable.Display(
                        data.frame(
                            Metric = c(
                                "Z-Bench — Overall, Long-Term (observed)",
                                "Z-Bench — Overall, Short-Term (= Z_LT + 1.5)",
                                "Sigma Level — Overall, Long-Term",
                                "Sigma Level — Overall, Short-Term",
                                "DPMO — Overall, Long-Term",
                                "DPMO — Overall, Short-Term (derived from Z_ST)",
                                "Yield — Overall, Long-Term",
                                "Yield — Overall, Short-Term"),
                            Value = c(
                                formatC(z_lt,  4, format="f"),
                                formatC(z_st,  4, format="f"),
                                formatC(z_lt,  4, format="f"),
                                formatC(z_st,  4, format="f"),
                                formatC(dpmo_lt, 2, format="f"),
                                formatC(dpmo_st, 2, format="f"),
                                sprintf("%.6f%%", yield_lt),
                                sprintf("%.6f%%", yield_st)),
                            stringsAsFactors = FALSE),
                        title   = gtxtf("Benchmark Sigma — %s", varlab),
                        caption = "Overall basis; ST = LT + 1.5σ")
                }
            }

            # ── Distribution shape ────────────────────────────────────────
            skew_val <- NA_real_; kurt_val <- NA_real_
            if (do_skewness) {
                # NOTE: when e1071 isn't installed, the fallback below replicates
                # e1071's TYPE-2 (bias-corrected Fisher-Pearson G1/G2) estimators
                # — the same definition used by standard SPC software — rather than the simpler
                # biased "type 1" moment ratios, so results stay consistent
                # regardless of which code path executes.
                skew_val <- tryCatch({
                    if (has_e1071) e1071::skewness(data, type=2)
                    else if (n > 2) {
                        m2 <- mean((data-xbar)^2); m3 <- mean((data-xbar)^3)
                        g1 <- m3 / m2^1.5
                        g1 * sqrt(n*(n-1)) / (n-2)              # G1, bias-corrected
                    } else NA_real_
                }, error = function(e) NA_real_)
                kurt_val <- tryCatch({
                    if (has_e1071) e1071::kurtosis(data, type=2)
                    else if (n > 3) {
                        m2 <- mean((data-xbar)^2); m4 <- mean((data-xbar)^4)
                        g2 <- m4 / m2^2 - 3
                        ((n+1)*g2 + 6) * (n-1) / ((n-2)*(n-3)) # G2, bias-corrected
                    } else NA_real_
                }, error = function(e) NA_real_)

                sk_interp <- function(v) if (is.na(v)) "N/A"
                                         else if (abs(v)<=0.5) "Approximately Symmetric"
                                         else if (v>0) sprintf("Right-Skewed (%.3f)", v)
                                         else           sprintf("Left-Skewed  (%.3f)", v)
                ku_interp <- function(v) if (is.na(v)) "N/A"
                                         else if (abs(v)<=0.5) "Mesokurtic"
                                         else if (v>0) "Leptokurtic — heavy tails"
                                         else           "Platykurtic — light tails"

                spsspivottable.Display(
                    data.frame(
                        Statistic      = c("Skewness (Fisher)", "Excess Kurtosis"),
                        Value          = c(if (!is.na(skew_val)) formatC(skew_val,4,format="f") else "N/A",
                                           if (!is.na(kurt_val)) formatC(kurt_val,4,format="f") else "N/A"),
                        Interpretation = c(sk_interp(skew_val), ku_interp(kurt_val)),
                        stringsAsFactors = FALSE),
                    title   = gtxtf("Distribution Shape — %s%s", varlab, transform_label),
                    caption = if (transform_ok) paste0(
                        "⚠ z-scores are symmetric by construction — skewness ≈0, kurtosis ≈0 is expected (",
                        pit$best_label, " transform)") else "")
            }

            # ── Percentiles ───────────────────────────────────────────────
            if (do_percentiles) {
                probs <- c(0.0027,0.01,0.05,0.10,0.25,0.50,0.75,0.90,0.95,0.99,0.9973)
                labs  <- c("P0.27% (−3σ)","P1%","P5%","P10%","P25% (Q1)","P50% (Median)",
                           "P75% (Q3)","P90%","P95%","P99%","P99.73% (+3σ)")
                spsspivottable.Display(
                    data.frame(Percentile=labs, Value=round(quantile(data,probs=probs),6),
                               stringsAsFactors=FALSE, row.names=NULL),
                    title   = gtxtf("Percentiles — %s%s", varlab, transform_label),
                    caption = paste0("Empirical — Type 7 interpolation (R default)",
                        if (transform_ok) "\n⚠ Values are on the z-score scale" else ""))
            }

            # ── I-MR Control Limits ───────────────────────────────────────
            # Formulas: UCL_I = x̄ + 2.66*MR̄  (2.66 = 3/d2 where d2=1.128 for n=2)
            #           LCL_I = x̄ - 2.66*MR̄
            #           UCL_MR = D4*MR̄ = 3.267*MR̄  (D4 for n=2)
            imr_data <- NULL
            if (do_controlimr && n >= 2) {
                mr      <- abs(diff(data))
                mr_bar  <- mean(mr)
                i_ucl   <- xbar + 2.66 * mr_bar
                i_lcl   <- xbar - 2.66 * mr_bar
                mr_ucl  <- 3.267 * mr_bar
                oc_i    <- sum(data > i_ucl | data < i_lcl)
                oc_mr   <- sum(mr  > mr_ucl)
                imr_data <- list(mr=mr, mr_bar=mr_bar, i_ucl=i_ucl, i_lcl=i_lcl,
                                 mr_ucl=mr_ucl, oc_i=oc_i, oc_mr=oc_mr)

                spsspivottable.Display(
                    data.frame(
                        Chart = c("I","I","I","MR","MR","MR"),
                        Limit = c("Center (x̄)","UCL (x̄+2.66·MR̄)","LCL (x̄−2.66·MR̄)",
                                  "Center (MR̄)","UCL (3.267·MR̄)","LCL"),
                        Value = round(c(xbar,i_ucl,i_lcl,mr_bar,mr_ucl,0),6),
                        stringsAsFactors=FALSE),
                    title   = gtxtf("I-MR Control Limits — %s", varlab),
                    caption = paste0(gtxtf("I-chart: %d out-of-control  |  MR-chart: %d above UCL",
                                    oc_i, oc_mr),
                        if (transform_ok) "\n⚠ Limits computed on z-score scale" else ""))
            }

            # ── Grubbs outlier test ───────────────────────────────────────
            if (do_outliers && n >= 3 && is.finite(s_total) && s_total > 0) {
                G       <- max(abs(data - xbar)) / s_total
                t_crit  <- qt(1 - 0.05/(2*n), df = n-2)
                G_crit  <- ((n-1)/sqrt(n)) * sqrt(t_crit^2 / (n-2+t_crit^2))
                det     <- G > G_crit
                out_idx <- which.max(abs(data - xbar))
                spsspivottable.Display(
                    data.frame(
                        Test  = c("Grubbs G Statistic","Critical Value (α=0.05)",
                                  "Outlier Detected?","Suspected Value","Observation #"),
                        Value = c(formatC(G,4,format="f"), formatC(G_crit,4,format="f"),
                                  if(det) "YES — investigate" else "No",
                                  if(det) formatC(data[out_idx],4,format="f") else "—",
                                  if(det) as.character(out_idx)                else "—"),
                        stringsAsFactors=FALSE),
                    title   = gtxtf("Grubbs Outlier Test — %s", varlab),
                    caption = "H₀: No outlier  |  α = 0.05  |  Tests single most extreme point")
            }

            # ── Confidence intervals ──────────────────────────────────────
            ci_rows <- list()
            add_ci  <- function(nm, lo, hi)
                ci_rows[[length(ci_rows)+1]] <<-
                    data.frame(Parameter=nm, Lower=round(lo,6), Upper=round(hi,6),
                               stringsAsFactors=FALSE)

            if (do_cimean) {
                t_cv <- qt(1-(1-conf)/2, df=n-1)
                mg   <- t_cv * s_total / sqrt(n)
                add_ci(sprintf("Process Mean (%.0f%% CI)", conf*100), xbar-mg, xbar+mg)
            }
            if (do_cistd) {
                chi_lo <- qchisq((1-conf)/2,     df=n-1)
                chi_hi <- qchisq(1-(1-conf)/2,   df=n-1)
                add_ci(sprintf("Std Deviation (%.0f%% CI)", conf*100),
                       sqrt((n-1)*s_total^2/chi_hi), sqrt((n-1)*s_total^2/chi_lo))
            }
            if (do_cicpk && !is.na(cpk)) {
                se_cpk <- sqrt(1/(9*n) + cpk^2/(2*(n-1)))
                z_cv   <- qnorm(1-(1-conf)/2)
                add_ci(sprintf("Cpk (%.0f%% CI, Bissell approx.)", conf*100),
                       cpk - z_cv*se_cpk, cpk + z_cv*se_cpk)
            }
            if (length(ci_rows) > 0) {
                ci_tbl <- do.call(rbind, ci_rows); row.names(ci_tbl) <- NULL
                spsspivottable.Display(ci_tbl,
                    title   = gtxtf("Confidence Intervals — %s", varlab),
                    caption = gtxtf("%.0f%% confidence  |  n = %d", conf*100, n))
            }

            # ── Process Report Card ───────────────────────────────────────
            if (do_reportcard) {
                rc_rows <- list()
                add_rc  <- function(nm, v) {
                    g <- grade_cap(v)
                    rc_rows[[length(rc_rows)+1]] <<-
                        data.frame(Index   = nm,
                                   Value   = if (!is.na(v)) formatC(v,4,format="f") else "N/A",
                                   Grade   = g$grade,
                                   Action  = g$action,
                                   stringsAsFactors = FALSE)
                }
                add_rc("Cp  (Potential — Within)",  cp)
                add_rc("Cpk (Actual — Within)",     cpk)
                add_rc("Pp  (Potential — Overall)", pp)
                add_rc("Ppk (Actual — Overall)",    ppk)
                if (!is.na(cpm_val)) add_rc("Cpm (Taguchi)", cpm_val)
                if (!is.na(cp_bc))  add_rc(sprintf("Cp Box-Cox (λ=%.2f)", bc_lam), cp_bc)
                if (!is.na(cpk_bc)) add_rc(sprintf("Cpk Box-Cox (λ=%.2f)", bc_lam), cpk_bc)

                if (length(rc_rows) > 0) {
                    rc_tbl <- do.call(rbind, rc_rows); row.names(rc_tbl) <- NULL
                    spsspivottable.Display(rc_tbl,
                        title   = gtxtf("Process Capability Report Card — %s", varlab),
                        caption = paste0("Grading per AIAG convention\n",
                                         "Grade scale (Cpk):  ≥ 1.67 = Excellent  |  ≥ 1.33 = Capable  |  ≥ 1.00 = Marginal  |  < 1.00 = Not Capable"))
                }
            }

            # ── Phase-wise (group-wise) analysis ──────────────────────────
            # When a grouping variable with >1 distinct level is supplied,
            # recompute control limits and capability indices separately
            # for each phase — taken in order of first appearance in the
            # sequence (the natural "before/after" reading of a grouping
            # variable) — so users can see how the process shifted between
            # phases. Each phase's I-chart / MR-chart limits use the SAME
            # validated formulas as the overall (pooled) versions computed
            # above (x̄ ± 2.66·MR̄ for the I-chart, 3.267·MR̄ for the MR-chart
            # UCL — see imr_data above), and capability indices reuse the
            # same Cp/Cpk/Pp/Ppk formulas from the "Capability indices"
            # section — just evaluated on the phase's own data subset. This
            # keeps every phase mathematically consistent with the overall
            # analysis (per the "must be mathematically conforming" rule).
            phase_stats <- NULL
            if (has_groups && do_controlimr) {
                # A "phase" is a maximal CONTIGUOUS run of one groupvar level
                # in sequence order — not just every distinct level. This
                # correctly handles two real-world patterns with the same
                # mechanism: (a) classic before/after-style variables, where
                # each level naturally forms exactly one run, and (b) variables
                # that legitimately repeat in cycles (e.g., a rotating
                # subgroup/sample identifier such as Sample = Sachet1..Sachet5
                # recurring every few rows) — each occurrence becomes its own
                # phase rather than being merged into one misleading segment
                # that would span (and overlap) the entire sequence.
                valid_idx <- which(!is.na(groups))
                g_seq     <- groups[valid_idx]
                rl        <- rle(g_seq)
                run_ends  <- cumsum(rl$lengths)
                run_starts<- c(1L, head(run_ends, -1) + 1L)
                n_runs    <- length(rl$values)
                lvl_counts <- table(rl$values)

                phase_list <- list()
                for (ri in seq_len(n_runs)) {
                    p_idx  <- valid_idx[run_starts[ri]:run_ends[ri]]
                    p_data <- data[p_idx]
                    p_n    <- length(p_data)
                    if (p_n < 2) next

                    lvl_lab  <- as.character(rl$values[ri])
                    # Only number repeated occurrences ("Sachet1 (run 2)") —
                    # keep single-occurrence labels clean ("Before"/"After").
                    occ_no   <- sum(rl$values[seq_len(ri)] == rl$values[ri])
                    p_label  <- if (lvl_counts[[lvl_lab]] > 1)
                                    sprintf("%s (run %d)", lvl_lab, occ_no)
                                else lvl_lab

                    p_xbar  <- mean(p_data)
                    p_s     <- sd(p_data)
                    p_mr    <- abs(diff(p_data))
                    p_mrbar <- mean(p_mr)
                    p_i_ucl  <- p_xbar + 2.66 * p_mrbar
                    p_i_lcl  <- p_xbar - 2.66 * p_mrbar
                    p_mr_ucl <- 3.267 * p_mrbar

                    # Capability indices for this phase.
                    # Within-phase Cp/Cpk: MRbar/d2(2) — AIAG standard for individual data;
                    # falls back to p_s (overall phase SD) if MRbar is undefined (n<2 or constant).
                    # Pp/Ppk: overall phase SD (p_s) — identical to main-analysis convention.
                    p_s_within <- compute_mrbar_sd(p_data)
                    if (!is.finite(p_s_within) || p_s_within <= 0) p_s_within <- p_s

                    p_cp <- p_cpk <- p_cpl <- p_cpu <- NA_real_
                    p_pp <- p_ppk <- p_ppl <- p_ppu <- NA_real_
                    if (has_specs && p_s > 0) {
                        if (!is.null(lsl) && !is.null(usl)) {
                            p_cp  <- (usl - lsl) / (sigma_mult * p_s_within)
                            p_cpl <- (p_xbar - lsl) / (sigma_half * p_s_within)
                            p_cpu <- (usl - p_xbar) / (sigma_half * p_s_within)
                            p_cpk <- min(p_cpl, p_cpu)
                            p_pp  <- (usl - lsl) / (sigma_mult * p_s)
                            p_ppl <- (p_xbar - lsl) / (sigma_half * p_s)
                            p_ppu <- (usl - p_xbar) / (sigma_half * p_s)
                            p_ppk <- min(p_ppl, p_ppu)
                        } else if (!is.null(lsl)) {
                            p_cpl <- (p_xbar - lsl) / (sigma_half * p_s_within)
                            p_cpk <- p_cpl
                            p_ppl <- (p_xbar - lsl) / (sigma_half * p_s); p_ppk <- p_ppl
                        } else if (!is.null(usl)) {
                            p_cpu <- (usl - p_xbar) / (sigma_half * p_s_within)
                            p_cpk <- p_cpu
                            p_ppu <- (usl - p_xbar) / (sigma_half * p_s); p_ppk <- p_ppu
                        }
                    }

                    phase_list[[length(phase_list)+1]] <- list(
                        label = p_label, idx = p_idx, data = p_data, n = p_n,
                        xbar = p_xbar, s = p_s, mr = p_mr, mr_bar = p_mrbar,
                        i_ucl = p_i_ucl, i_lcl = p_i_lcl, mr_ucl = p_mr_ucl,
                        cp = p_cp, cpk = p_cpk, cpl = p_cpl, cpu = p_cpu,
                        pp = p_pp, ppk = p_ppk, ppl = p_ppl, ppu = p_ppu,
                        is_final = FALSE
                    )
                }
                # Highlight only the single most-recent phase overall (the
                # last qualifying run in sequence order) — not one per level.
                if (length(phase_list) > 0)
                    phase_list[[length(phase_list)]]$is_final <- TRUE
                if (length(phase_list) > 1) {
                    phase_stats <- phase_list
                } else {
                    # Tell the user WHY no phase output appears, rather than
                    # silently producing nothing — this happens when '%s'
                    # changes on (almost) every row, so it never forms a
                    # contiguous run of >= 2 observations. That is the
                    # signature of a per-observation/rotating subgroup tag
                    # (e.g., a 5-position sample identifier cycling every few
                    # rows) — a legitimate and still-honored input for
                    # RBAR/SBAR sigma estimation, but not something a
                    # phase/stage chart can meaningfully segment, since
                    # "phases" must each span a real stretch of the sequence.
                    warns$warn(gtxtf(paste(
                        "Phase analysis skipped for '%s': '%s' changes on",
                        "(nearly) every observation, so no value forms a",
                        "contiguous run of 2+ rows. It is being used as a",
                        "per-observation subgroup identifier (still driving",
                        "RBAR/SBAR sigma estimation as configured) rather",
                        "than a sequential phase indicator — phase/stage",
                        "charts need each phase to span a real stretch of",
                        "the sequence (e.g., Before/After, Batch 1 then",
                        "Batch 2)."), varlab, as.character(groupvar)))
                }
            }

            # ── Process Capability — By Phase table ───────────────────────
            # One row per phase, alongside (never replacing) the existing
            # overall "Process Capability Indices" table above. Cp/Cpk/Pp/Ppk
            # use the identical formulas, evaluated on each phase's own data
            # subset — see the phase_stats computation just above.
            if (!is.null(phase_stats) && has_specs) {
              tryCatch({
                ph_rows <- list()
                for (ph in phase_stats) {
                    ph_rows[[length(ph_rows)+1]] <- data.frame(
                        Phase  = ph$label,
                        N      = ph$n,
                        Mean   = round(ph$xbar, 4),
                        StdDev = round(ph$s, 4),
                        Cp     = if (!is.na(ph$cp))  formatC(ph$cp, 4, format="f")  else "N/A",
                        Cpk    = if (!is.na(ph$cpk)) formatC(ph$cpk,4, format="f") else "N/A",
                        Pp     = if (!is.na(ph$pp))  formatC(ph$pp, 4, format="f")  else "N/A",
                        Ppk    = if (!is.na(ph$ppk)) formatC(ph$ppk,4, format="f") else "N/A",
                        Grade  = if (!is.na(ph$cpk)) grade_cap(ph$cpk)$grade
                                 else if (!is.na(ph$cp)) grade_cap(ph$cp)$grade else "N/A",
                        stringsAsFactors = FALSE)
                }
                if (length(ph_rows) > 0) {
                    ph_tbl <- do.call(rbind, ph_rows); row.names(ph_tbl) <- NULL
                    spsspivottable.Display(ph_tbl,
                        title   = gtxtf("Process Capability — By Phase (%s) — %s%s",
                                        as.character(groupvar), varlab, transform_label),
                        caption = paste0(
                            gtxtf(
                                "%g-sigma  |  %d phases in sequence order  |  final phase = most recent data",
                                sigma_mult, length(phase_stats)),
                            if (transform_ok) "\n⚠ Phase indices on z-score scale" else ""))
                }
              }, error = function(e) {
                  .cl <- tryCatch(paste(deparse(conditionCall(e)), collapse=" "),
                                  error=function(e2) "<no call>")
                  warns$warn(sprintf(
                      "[DIAG] By-Phase capability table failed for '%s': %s | call: %s",
                      varlab, conditionMessage(e), .cl))
              })
            }

            # ── Store results for multi-var comparison and charts ─────────
            results[[v]] <- list(
                varlab   = varlab, data  = data, n = n,
                xbar     = xbar,  s     = s_total, s_within = s_within,
                cp = cp, cpk = cpk, cpl = cpl, cpu = cpu,
                pp = pp, ppk = ppk, cpl_o = ppl, cpu_o = ppu,
                cpm = cpm_val, cp_bc = cp_bc, cpk_bc = cpk_bc,
                ppm_total = ppm_total, ppm_below = ppm_below, ppm_above = ppm_above,
                pct_out = pct_out, n_below = n_below, n_above = n_above,
                z_bench = z_bench, groups = groups, imr_data = imr_data,
                phase_stats = phase_stats,
                skew = skew_val, kurt = kurt_val,
                orig_data     = orig_data,
                orig_lsl      = orig_lsl,
                orig_usl      = orig_usl,
                did_transform = transform_ok
            )
        } # end per-variable loop

        # ════════════════════════════════════════════════════════════════════
        # 5.3  MULTI-VARIABLE COMPARISON TABLE
        # ════════════════════════════════════════════════════════════════════
        if (multi_var && length(results) > 1) {
            fmt4 <- function(v) if (!is.na(v)) formatC(v, 4, format="f") else "N/A"
            comp <- data.frame(
                Variable  = sapply(results, `[[`, "varlab"),
                N         = sapply(results, `[[`, "n"),
                Mean      = sapply(results, function(r) round(r$xbar, 4)),
                StdDev    = sapply(results, function(r) round(r$s, 4)),
                Cp        = sapply(results, function(r) fmt4(r$cp)),
                Cpk       = sapply(results, function(r) fmt4(r$cpk)),
                Pp        = sapply(results, function(r) fmt4(r$pp)),
                Ppk       = sapply(results, function(r) fmt4(r$ppk)),
                `PPM Total` = sapply(results, function(r)
                                   if (!is.na(r$ppm_total))
                                       formatC(r$ppm_total, 2, format="f") else "N/A"),
                stringsAsFactors = FALSE, row.names = NULL, check.names = FALSE)
            spsspivottable.Display(comp,
                title   = "Multi-Variable Capability Comparison",
                caption = gtxtf("%g-sigma  |  LSL=%s  |  USL=%s",
                    sigma_mult,
                    if (!is.null(lsl)) as.character(lsl) else "N/A",
                    if (!is.null(usl)) as.character(usl) else "N/A"))
        }

        # ════════════════════════════════════════════════════════════════════
        # 5.4  CHARTS
        # All graphics wrapped in spssRGraphics.Submit() — appear in SPSS Viewer
        # ════════════════════════════════════════════════════════════════════

        if (create_charts && length(results) > 0) {

            # Load logo — try PNG then JPEG; warn but continue on failure.
            # Presence of a Logo Path alone determines whether a logo is used —
            # the user simply leaves the field blank to omit a logo.
            logo_img <- NULL
            if (!is.null(logopath) && nchar(trimws(logopath)) > 0) {
                lp <- trimws(logopath)
                if (!file.exists(lp)) {
                    warns$warn(gtxtf("Logo file not found: %s", lp))
                } else {
                    tryCatch({
                        if (grepl("\\.png$", lp, ignore.case = TRUE) && has_png)
                            logo_img <- png::readPNG(lp)
                        else if (grepl("\\.(jpg|jpeg)$", lp, ignore.case = TRUE) && has_jpeg)
                            logo_img <- jpeg::readJPEG(lp)
                        else
                            warns$warn("Logo format must be PNG or JPEG.")
                    }, error = function(e)
                        warns$warn(gtxtf("Logo not loaded: %s", e$message)))
                }
            }

            # Spec-limit drawing helpers
            draw_spec_v <- function() {
                if (!is.null(lsl))    abline(v=lsl,    col="#E74C3C", lwd=2, lty=2)
                if (!is.null(usl))    abline(v=usl,    col="#E74C3C", lwd=2, lty=2)
                if (!is.null(target)) abline(v=target, col="#27AE60", lwd=2, lty=3)
            }
            draw_spec_h <- function() {
                if (!is.null(lsl))    abline(h=lsl,    col="#E74C3C", lwd=2, lty=2)
                if (!is.null(usl))    abline(h=usl,    col="#E74C3C", lwd=2, lty=2)
                if (!is.null(target)) abline(h=target, col="#27AE60", lwd=2, lty=3)
            }
            spec_legend <- function(extra_nm=NULL, extra_col=NULL, extra_lty=NULL) {
                nm <- character(0); cl <- character(0); lt <- integer(0)
                if (!is.null(lsl))    { nm<-c(nm,"LSL");    cl<-c(cl,"#E74C3C"); lt<-c(lt,2L) }
                if (!is.null(usl))    { nm<-c(nm,"USL");    cl<-c(cl,"#E74C3C"); lt<-c(lt,2L) }
                if (!is.null(target)) { nm<-c(nm,"Target"); cl<-c(cl,"#27AE60"); lt<-c(lt,3L) }
                if (!is.null(extra_nm)) { nm<-c(extra_nm,nm); cl<-c(extra_col,cl); lt<-c(extra_lty,lt) }
                if (length(nm)>0)
                    legend("topright", legend=nm, col=cl, lty=lt, lwd=2, cex=0.75, bty="n")
            }

            outer_title <- if (!is.null(company) && nchar(company)>0)
                               paste0("Process Capability Analysis  —  ", company)
                           else "Process Capability Analysis"
            if (!is.null(preparedby) && nchar(preparedby)>0)
                outer_title <- paste0(outer_title, "  |  Prepared by: ", preparedby)

            # ── Use first variable as "primary" for single-var charts ─────
            # Chart title suffix: single var name when n_vars==1, else "All Variables"
            all_vars_lbl <- if (n_vars > 1) "All Variables" else results[[1]]$varlab
            r1          <- results[[1]]
            data1       <- r1$data
            n1          <- r1$n
            xbar1       <- r1$xbar
            s1          <- r1$s
            cp1         <- r1$cp;  cpk1 <- r1$cpk
            cpl1        <- r1$cpl; cpu1 <- r1$cpu
            pp1         <- r1$pp;  ppk1 <- r1$ppk
            cpm1        <- r1$cpm
            ppt1        <- r1$ppm_total
            first_varlab <- r1$varlab

            # ─────────────────────────────────────────────────────────────
            # CHART SET 1: Main 2×3 panel
            # Panel 1: Enhanced standard SPC-style capability histogram (ALL indices)
            # ─────────────────────────────────────────────────────────────
            tryCatch({
                spssRGraphics.Submit({
                    op <- par(mfrow=c(2,3), mar=c(3.2,3.8,2.5,0.8),
                              oma=c(0,0,3,0), cex.lab=0.95, cex.axis=0.85,
                              cex.main=0.95, font.main=2, lwd=1.2)

                    # ── Panel 1: Capability Histogram (standard SPC-style — all indices) ──
                    if (ch_histogram) {
                        # For multi-var: overlay; for single: standard
                        all_vals <- unlist(lapply(results, `[[`, "data"))
                        xlim <- range(c(all_vals, lsl, usl), na.rm=TRUE)
                        pad  <- diff(xlim) * 0.12; xlim <- xlim + c(-pad, pad)
                        nbr  <- max(5L, min(30L, ceiling(sqrt(n1))))

                        # Base histogram (first variable)
                        hist(data1, breaks=nbr, freq=FALSE,
                             col="#AED6F1", border="white",
                             main="Capability Histogram", xlab="Measurement", ylab="Density",
                             xlim=xlim)

                        # Normal curves for all variables
                        for (vi in seq_along(results)) {
                            ri   <- results[[vi]]
                            col_i <- VAR_PALETTE[((vi-1) %% length(VAR_PALETTE)) + 1]
                            xseq <- seq(xlim[1], xlim[2], length.out=400)
                            lines(xseq, dnorm(xseq, ri$xbar, ri$s),
                                  col=col_i, lwd=ifelse(multi_var, 2, 2.5))
                            if (multi_var && vi > 1)
                                hist(ri$data, breaks=nbr, freq=FALSE, add=TRUE,
                                     col=paste0(col_i, "44"), border="white")
                        }

                        draw_spec_v()

                        # ── Standard SPC-style all-indices stats box ─────────────────
                        cap_lines <- c(
                            sprintf("n    = %d",     n1),
                            sprintf("Mean = %.4f",   xbar1),
                            sprintf("s    = %.4f",   s1))
                        if (!is.null(lsl)) cap_lines <- c(cap_lines, sprintf("LSL  = %.4f", lsl))
                        if (!is.null(usl)) cap_lines <- c(cap_lines, sprintf("USL  = %.4f", usl))
                        cap_lines <- c(cap_lines, "--- Within ---",
                            safe_idx("Cp",  cp1),  safe_idx("Cpl", cpl1),
                            safe_idx("Cpu", cpu1),  safe_idx("Cpk", cpk1),
                            "--- Overall ---",
                            safe_idx("Pp",  pp1),   safe_idx("Ppk", ppk1))
                        if (!is.null(cpm1) && length(cpm1)==1 && !is.na(cpm1))
                            cap_lines <- c(cap_lines, safe_idx("Cpm", cpm1))
                        if (!is.null(ppt1) && length(ppt1)==1 && !is.na(ppt1))
                            cap_lines <- c(cap_lines, sprintf("PPM  = %.1f", ppt1))

                        # Stats box — place in bottom-right to avoid covering bars
                        legend("topleft", legend=cap_lines,
                               bty="o", bg=adjustcolor("#FFFEF0", alpha.f=0.92),
                               cex=0.63, box.lwd=0.8, text.col="#2C3E50",
                               inset=c(0.01, 0.01))

                        if (multi_var)
                            legend("topright",
                                   legend = sapply(results, `[[`, "varlab"),
                                   col    = VAR_PALETTE[seq_along(results)],
                                   lty=1, lwd=2, cex=0.7, bty="n")

                        add_logo_to_plot(logo_img)
                        grid(col="grey92", lty=1, lwd=0.5)
                    } else plot.new()

                    # ── Panel 2: Normal Probability Plot ──────────────────
                    if (ch_normalprob) {
                        # Compute unified axis range for all variables
                        all_qq <- lapply(results, function(ri)
                                     qqnorm(ri$data, plot.it=FALSE))
                        qq_xlim <- range(sapply(all_qq, function(q) range(q$x)))
                        qq_ylim <- range(sapply(all_qq, function(q) range(q$y)))

                        plot(all_qq[[1]]$x, all_qq[[1]]$y,
                             main="Normal Probability Plot",
                             xlab="Theoretical Quantiles", ylab="Sample Quantiles",
                             pch=19, col="#2980B9", cex=0.65,
                             xlim=qq_xlim, ylim=qq_ylim)
                        qqline(data1, col="#E74C3C", lwd=2, lty=2)

                        # Overlay additional variables (add=TRUE via points, not qqnorm)
                        if (multi_var) for (vi in seq_along(results)[-1]) {
                            ri    <- results[[vi]]
                            col_i <- VAR_PALETTE[((vi-1)%%length(VAR_PALETTE))+1]
                            qq_i  <- all_qq[[vi]]
                            points(qq_i$x, qq_i$y, pch=19, col=col_i, cex=0.5)
                        }
                        grid(col="grey92", lty=1, lwd=0.5)
                        if (multi_var)
                            legend("topleft",
                                   legend=sapply(results, `[[`, "varlab"),
                                   col=VAR_PALETTE[seq_along(results)],
                                   pch=19, lty=NA, cex=0.7, bty="n")
                        else
                            legend("topleft",
                                   legend=c("Sample","Reference Line"),
                                   pch=c(19,NA), lty=c(NA,2),
                                   col=c("#2980B9","#E74C3C"), lwd=c(NA,2), cex=0.75, bty="n")
                    } else plot.new()

                    # ── Panel 3: Process vs Specification ─────────────────
                    if (ch_capability) {
                        span  <- max(4.5*s1,
                                     if (!is.null(lsl)) abs(xbar1-lsl)+s1 else 0,
                                     if (!is.null(usl)) abs(usl-xbar1)+s1 else 0)
                        xseq  <- seq(xbar1-span, xbar1+span, length.out=400)
                        xlim4 <- range(c(xseq, lsl, usl), na.rm=TRUE)

                        # Multi-variable: overlay all normal curves
                        y_max <- 0
                        for (ri in results) y_max <- max(y_max, dnorm(ri$xbar, ri$xbar, ri$s))
                        ylim4 <- c(0, y_max * 1.15)

                        plot(xseq, dnorm(xseq, xbar1, s1), type="n",
                             main="Process vs Specification",
                             xlab="Measurement", ylab="Density",
                             xlim=xlim4, ylim=ylim4)

                        for (vi in rev(seq_along(results))) {
                            ri    <- results[[vi]]
                            col_i <- VAR_PALETTE[((vi-1)%%length(VAR_PALETTE))+1]
                            xs    <- seq(xlim4[1], xlim4[2], length.out=400)
                            ys    <- dnorm(xs, ri$xbar, ri$s)
                            polygon(c(xs, rev(xs)), c(ys, rep(0,length(ys))),
                                    col=paste0(col_i,"44"), border=NA)
                            lines(xs, ys, col=col_i, lwd=2)
                            abline(v=ri$xbar, col=col_i, lwd=1.5, lty=3)
                        }
                        draw_spec_v()
                        if (!is.na(cpk1))
                            legend("topleft",
                                   legend=sprintf("Cpk = %.4f", cpk1),
                                   bty="o", bg="#FFFFF0", cex=0.8, box.lwd=0.5)
                        grid(col="grey92", lty=1, lwd=0.5)
                    } else plot.new()

                    # ── Panel 4: Run Chart ─────────────────────────────────
                    if (ch_run) {
                        y_range <- range(c(data1, lsl, usl), na.rm=TRUE)
                        plot(seq_len(n1), data1, type="o",
                             pch=19, col="#2980B9", cex=0.6, lwd=1.5,
                             main="Run Chart", xlab="Observation Order", ylab="Measurement",
                             ylim=y_range + diff(y_range)*c(-0.05,0.05))
                        abline(h=xbar1, col="#27AE60", lwd=2)
                        draw_spec_h()
                        # Highlight out-of-spec points
                        ooc <- integer(0)
                        if (!is.null(lsl)) ooc <- c(ooc, which(data1 < lsl))
                        if (!is.null(usl)) ooc <- c(ooc, which(data1 > usl))
                        if (length(unique(ooc)) > 0)
                            points(unique(ooc), data1[unique(ooc)],
                                   pch=21, cex=1.8, col="#E74C3C", bg="#FDEDEC", lwd=2)
                        spec_legend(extra_nm="Mean", extra_col="#27AE60", extra_lty=1L)
                        grid(col="grey92", lty=1, lwd=0.5)
                    } else plot.new()

                    # ── Panel 5: Box Plot ──────────────────────────────────
                    if (ch_boxplot) {
                        tryCatch({
                            if (multi_var) {
                                all_data_list <- lapply(results, `[[`, "data")
                                labs_bp       <- sapply(results, `[[`, "varlab")
                                boxplot(all_data_list,
                                        col    = VAR_PALETTE[((seq_along(results)-1) %% length(VAR_PALETTE)) + 1],
                                        border = "#2C3E50",
                                        names  = labs_bp,
                                        main   = "Box Plot", ylab = "Measurement",
                                        las    = 2, cex.axis = 0.7)
                            } else {
                                boxplot(data1, col="#AED6F1", border="#2C3E50",
                                        main="Box Plot", ylab="Measurement",
                                        pch=19, outcex=0.8, outcol="#E74C3C")
                            }
                            draw_spec_h()
                            spec_legend()
                            grid(col="grey92", lty=1, lwd=0.5)
                        }, error = function(e) {
                            plot.new(); plot.window(xlim=c(0,1), ylim=c(0,1))
                            title(main="Box Plot", col.main="#2C3E50")
                            text(0.5, 0.5,
                                 paste0("Box Plot could not be drawn:\n", conditionMessage(e)),
                                 col="#E74C3C", cex=0.65)
                        })
                    } else plot.new()

                    # ── Panel 6: Text Summary ──────────────────────────────
                    if (ch_summary) {
                        plot.new(); plot.window(xlim=c(0,1), ylim=c(0,1))
                        title(main="Capability Summary", col.main="#2C3E50")
                        rect(0.03, 0.03, 0.97, 0.97, col="#FAFAFA", border="#BDC3C7")

                        sum_txt <- c(
                            "── PROCESS ──",
                            sprintf("n = %d",    n1),
                            sprintf("Mean  = %.4f", xbar1),
                            sprintf("StdDev = %.4f", s1),
                            "",
                            "── LIMITS ──",
                            sprintf("LSL = %s", if (!is.null(lsl)) formatC(lsl,4,format="f") else "N/A"),
                            sprintf("USL = %s", if (!is.null(usl)) formatC(usl,4,format="f") else "N/A"),
                            sprintf("Target = %s", if (!is.null(target)) formatC(target,4,format="f") else "N/A"),
                            "",
                            "── WITHIN ──",
                            sprintf("Cp  = %s", if (!is.na(cp1))  formatC(cp1, 4,format="f") else "N/A"),
                            sprintf("Cpk = %s", if (!is.na(cpk1)) formatC(cpk1,4,format="f") else "N/A"),
                            "",
                            "── OVERALL ──",
                            sprintf("Pp  = %s", if (!is.na(pp1))  formatC(pp1, 4,format="f") else "N/A"),
                            sprintf("Ppk = %s", if (!is.na(ppk1)) formatC(ppk1,4,format="f") else "N/A"),
                            sprintf("Cpm = %s", if (!is.na(cpm1)) formatC(cpm1,4,format="f") else "N/A"),
                            "",
                            "── PERFORMANCE ──",
                            sprintf("Out = %.4f%%", r1$pct_out),
                            sprintf("PPM = %s",
                                if (!is.na(ppt1)) formatC(ppt1,2,format="f") else "N/A")
                        )
                        if (!is.null(preparedby) && nchar(preparedby)>0)
                            sum_txt <- c(sum_txt, "", sprintf("By: %s", preparedby))
                        if (!is.null(company) && nchar(company)>0)
                            sum_txt <- c(sum_txt, sprintf("Co: %s", company))

                        text(0.5, 0.95, paste(sum_txt, collapse="\n"),
                             cex=0.6, adj=c(0.5,1), col="#2C3E50")
                        # Logo — top-right corner of summary panel (safe: inside xlim 0-1 space)
                        if (!is.null(logo_img)) tryCatch({
                            img_h <- if (is.array(logo_img)) dim(logo_img)[1] else nrow(logo_img)
                            img_w <- if (is.array(logo_img)) dim(logo_img)[2] else ncol(logo_img)
                            lw <- 0.28; lh <- lw * img_h / img_w
                            rasterImage(logo_img, 1-lw, 1-lh-0.01, 1, 0.99, interpolate=TRUE)
                        }, error=function(e) invisible(NULL))
                    } else plot.new()

                    mtext(outer_title, outer=TRUE, cex=1.05, font=2, col="#2C3E50")
                    par(op)   # Reset par immediately — not via on.exit
                })
            }, error=function(e) warns$warn(gtxtf("Main panel error: %s", e$message)))

            # ─────────────────────────────────────────────────────────────
            # CHART SET 1b: ggplot2 HIGH-QUALITY CHARTS (when ggplot2 available)
            # These produce publication-quality charts displayed in SPSS Viewer
            # ─────────────────────────────────────────────────────────────
            if (has_ggplot2) {
                tryCatch({
                    theme_cap <- ggplot2::theme_minimal(base_size = 11) +
                        ggplot2::theme(
                            plot.title       = ggplot2::element_text(face="bold", size=12, hjust=0.5),
                            panel.grid.minor = ggplot2::element_blank(),
                            panel.grid.major = ggplot2::element_line(color="grey92"),
                            strip.text       = ggplot2::element_text(face="bold"),
                            legend.position  = "bottom",
                            legend.key.size  = ggplot2::unit(0.5,"cm")
                        )

                    # Build combined data frame for multi-variable plots
                    plot_df <- do.call(rbind, lapply(names(results), function(vv) {
                        data.frame(value = results[[vv]]$data,
                                   variable = results[[vv]]$varlab,
                                   stringsAsFactors = FALSE)
                    }))

                    # ── ggplot: Overlay Density + Spec ──────────────────────
                    p_dens <- ggplot2::ggplot(plot_df,
                                 ggplot2::aes(x=value, fill=variable, color=variable)) +
                        ggplot2::geom_histogram(ggplot2::aes(y=ggplot2::after_stat(density)),
                                     alpha=0.35, bins=max(8, ceiling(sqrt(n1))),
                                     position="identity") +
                        ggplot2::geom_density(alpha=0, linewidth=1.2) +
                        ggplot2::scale_fill_manual(values=VAR_PALETTE) +
                        ggplot2::scale_color_manual(values=VAR_PALETTE) +
                        ggplot2::labs(title=paste0("Capability Histogram — ", all_vars_lbl),
                                      x="Measurement", y="Density",
                                      fill="Variable", color="Variable") +
                        theme_cap

                    # Add spec limit lines
                    if (!is.null(lsl))    p_dens <- p_dens +
                        ggplot2::geom_vline(xintercept=lsl,    color="#E74C3C", linewidth=1.2, linetype="dashed") +
                        ggplot2::annotate("text", x=lsl, y=Inf, label="LSL", color="#E74C3C",
                                          vjust=1.5, hjust=-0.2, size=3.5)
                    if (!is.null(usl))    p_dens <- p_dens +
                        ggplot2::geom_vline(xintercept=usl,    color="#E74C3C", linewidth=1.2, linetype="dashed") +
                        ggplot2::annotate("text", x=usl, y=Inf, label="USL", color="#E74C3C",
                                          vjust=1.5, hjust=1.2, size=3.5)
                    if (!is.null(target)) p_dens <- p_dens +
                        ggplot2::geom_vline(xintercept=target, color="#27AE60", linewidth=1.2, linetype="dotted") +
                        ggplot2::annotate("text", x=target, y=Inf, label="Target", color="#27AE60",
                                          vjust=1.5, hjust=-0.2, size=3.5)

                    spssRGraphics.Submit({
                        print(p_dens)
                        if (!is.null(logo_img)) tryCatch(
                            grid::grid.raster(logo_img, x=0.97, y=0.97,
                                width=grid::unit(0.08,"npc"), height=grid::unit(0.07,"npc"),
                                just=c("right","top")),
                            error=function(e) invisible(NULL))
                    })

                    # ── ggplot: Box Plot ──────────────────────────────────────
                    tryCatch({
                        pal_box <- VAR_PALETTE[((seq_along(unique(plot_df$variable))-1) %% length(VAR_PALETTE)) + 1]
                        p_box <- ggplot2::ggplot(plot_df,
                                     ggplot2::aes(x=variable, y=value, fill=variable)) +
                            ggplot2::geom_boxplot(alpha=0.7, outlier.color="#E74C3C",
                                                  outlier.shape=21, outlier.size=2,
                                                  outlier.fill="#FDEDEC") +
                            ggplot2::geom_jitter(width=0.15, size=0.8, alpha=0.25, color="#2C3E50") +
                            ggplot2::scale_fill_manual(values=pal_box, guide="none") +
                            ggplot2::labs(title=paste0("Box Plot — ", all_vars_lbl),
                                          x="Variable", y="Measurement") +
                            theme_cap

                        if (!is.null(lsl))    p_box <- p_box +
                            ggplot2::geom_hline(yintercept=lsl, color="#E74C3C", linewidth=1, linetype="dashed")
                        if (!is.null(usl))    p_box <- p_box +
                            ggplot2::geom_hline(yintercept=usl, color="#E74C3C", linewidth=1, linetype="dashed")
                        if (!is.null(target)) p_box <- p_box +
                            ggplot2::geom_hline(yintercept=target, color="#27AE60", linewidth=1, linetype="dotted")

                        spssRGraphics.Submit({
                            print(p_box)
                            if (!is.null(logo_img)) tryCatch(
                                grid::grid.raster(logo_img, x=0.97, y=0.97,
                                    width=grid::unit(0.08,"npc"), height=grid::unit(0.07,"npc"),
                                    just=c("right","top")),
                                error=function(e) invisible(NULL))
                        })
                    }, error = function(e) {
                        tryCatch(spssRGraphics.Submit({
                            plot.new(); plot.window(xlim=c(0,1), ylim=c(0,1))
                            title(main="Box Plot — All Variables", col.main="#2C3E50")
                            text(0.5, 0.5,
                                 paste0("Box Plot could not be drawn:\n", conditionMessage(e)),
                                 col="#E74C3C", cex=0.8)
                        }), error = function(e2) invisible(NULL))
                    })

                    # ── ggplot: Normal Q-Q with confidence band ───────────────
                    p_qq <- ggplot2::ggplot(plot_df,
                                ggplot2::aes(sample=value, color=variable)) +
                        ggplot2::stat_qq(size=1.5, alpha=0.7) +
                        ggplot2::stat_qq_line(linewidth=1, linetype="dashed") +
                        ggplot2::scale_color_manual(values=VAR_PALETTE) +
                        ggplot2::labs(title=paste0("Normal Q-Q Plot — ", all_vars_lbl),
                                      x="Theoretical Quantiles", y="Sample Quantiles",
                                      color="Variable") +
                        theme_cap
                    spssRGraphics.Submit({
                        print(p_qq)
                        if (!is.null(logo_img)) tryCatch(
                            grid::grid.raster(logo_img, x=0.97, y=0.97,
                                width=grid::unit(0.08,"npc"), height=grid::unit(0.07,"npc"),
                                just=c("right","top")),
                            error=function(e) invisible(NULL))
                    })

                    # ── ggplot: Run Chart ─────────────────────────────────────
                    run_df <- do.call(rbind, lapply(names(results), function(vv) {
                        ri <- results[[vv]]
                        data.frame(obs=seq_len(ri$n), value=ri$data,
                                   variable=ri$varlab, mean=ri$xbar,
                                   stringsAsFactors=FALSE)
                    }))
                    p_run <- ggplot2::ggplot(run_df,
                                 ggplot2::aes(x=obs, y=value, color=variable, group=variable)) +
                        ggplot2::geom_line(alpha=0.8) +
                        ggplot2::geom_point(size=1.5, alpha=0.7) +
                        ggplot2::geom_hline(data=unique(run_df[,c("variable","mean")]),
                                            ggplot2::aes(yintercept=mean, color=variable),
                                            linetype="dotted", linewidth=0.8) +
                        ggplot2::scale_color_manual(values=VAR_PALETTE) +
                        ggplot2::labs(title="Run Chart", x="Observation Order",
                                      y="Measurement", color="Variable") +
                        theme_cap
                    if (!is.null(lsl)) p_run <- p_run +
                        ggplot2::geom_hline(yintercept=lsl, color="#E74C3C", linewidth=1, linetype="dashed")
                    if (!is.null(usl)) p_run <- p_run +
                        ggplot2::geom_hline(yintercept=usl, color="#E74C3C", linewidth=1, linetype="dashed")
                    spssRGraphics.Submit({
                        print(p_run)
                        if (!is.null(logo_img)) tryCatch(
                            grid::grid.raster(logo_img, x=0.97, y=0.97,
                                width=grid::unit(0.08,"npc"), height=grid::unit(0.07,"npc"),
                                just=c("right","top")),
                            error=function(e) invisible(NULL))
                    })

                    # ── ggplot: Capability Index Comparison (multi-var) ───────
                    if (multi_var && has_specs) {
                        idx_df <- do.call(rbind, lapply(names(results), function(vv) {
                            ri <- results[[vv]]
                            data.frame(
                                variable = ri$varlab,
                                Index    = c("Cp","Cpk","Pp","Ppk"),
                                Value    = c(
                                    ifelse(is.na(ri$cp),  NA_real_, ri$cp),
                                    ifelse(is.na(ri$cpk), NA_real_, ri$cpk),
                                    ifelse(is.na(ri$pp),  NA_real_, ri$pp),
                                    ifelse(is.na(ri$ppk), NA_real_, ri$ppk)),
                                stringsAsFactors=FALSE)
                        }))
                        idx_df <- idx_df[!is.na(idx_df$Value),]
                        if (nrow(idx_df) > 0) {
                            p_idx <- ggplot2::ggplot(idx_df,
                                        ggplot2::aes(x=Index, y=Value, fill=variable)) +
                                ggplot2::geom_col(position="dodge", alpha=0.85, width=0.65) +
                                ggplot2::geom_hline(yintercept=1.33, color="#E74C3C",
                                                    linetype="dashed", linewidth=0.9) +
                                ggplot2::geom_hline(yintercept=1.00, color="#F39C12",
                                                    linetype="dashed", linewidth=0.9) +
                                ggplot2::annotate("text", x=0.6, y=1.33, label="Capable (1.33)",
                                                  color="#E74C3C", hjust=0, size=3) +
                                ggplot2::annotate("text", x=0.6, y=1.00, label="Marginal (1.00)",
                                                  color="#F39C12", hjust=0, size=3) +
                                ggplot2::scale_fill_manual(values=VAR_PALETTE) +
                                ggplot2::labs(title="Capability Index Comparison",
                                              x="Index", y="Value", fill="Variable") +
                                theme_cap
                            spssRGraphics.Submit({
                                print(p_idx)
                        if (!is.null(logo_img)) tryCatch(
                            grid::grid.raster(logo_img, x=0.97, y=0.97,
                                width=grid::unit(0.08,"npc"), height=grid::unit(0.07,"npc"),
                                just=c("right","top")),
                            error=function(e) invisible(NULL))
                            })
                        }
                    }

                }, error = function(e)
                    warns$warn(gtxtf("ggplot2 chart error: %s", e$message)))
            }

            # ─────────────────────────────────────────────────────────────
            # CHART SET 2: I-MR Control Charts — one chart per variable
            # ─────────────────────────────────────────────────────────────
            if (ch_imr) {
                for (vi in seq_along(results)) {
                    rv    <- results[[vi]]
                    if (is.null(rv$imr_data) || rv$n < 2) next
                    local({
                        ri   <- rv
                        imrd <- ri$imr_data
                        mr   <- imrd$mr
                        dat  <- ri$data
                        n_i  <- ri$n
                        tryCatch({
                            # ── I-chart (separate Submit for full height) ──────
                            i_ooc   <- which(dat > imrd$i_ucl | dat < imrd$i_lcl)
                            ylim_i  <- range(c(dat, imrd$i_ucl, imrd$i_lcl,
                                               lsl, usl), na.rm=TRUE)
                            ylim_i  <- ylim_i + diff(ylim_i)*0.10*c(-1,1)
                            spec_lbl <- character(0); spec_col <- character(0); spec_lty <- integer(0)
                            if (!is.null(lsl)) { spec_lbl<-c(spec_lbl,sprintf("LSL=%.4f",lsl)); spec_col<-c(spec_col,"#9B59B6"); spec_lty<-c(spec_lty,3L) }
                            if (!is.null(usl)) { spec_lbl<-c(spec_lbl,sprintf("USL=%.4f",usl)); spec_col<-c(spec_col,"#9B59B6"); spec_lty<-c(spec_lty,3L) }

                            spssRGraphics.Submit({
                                graphics::layout(1); par(mfrow=c(1,1))
                                op_i <- par(mar=c(4,4.5,3.5,2))
                                on.exit(par(op_i), add=TRUE)
                                plot(seq_len(n_i), dat, type="o",
                                     pch=19, col="#2980B9", cex=0.7, lwd=1.5,
                                     main=paste0("Individuals (I) Chart  —  ", ri$varlab),
                                     xlab="Observation", ylab="Individual Value",
                                     ylim=ylim_i)
                                abline(h=ri$xbar,    col="#27AE60", lwd=2)
                                abline(h=imrd$i_ucl, col="#E74C3C", lwd=2, lty=2)
                                abline(h=imrd$i_lcl, col="#E74C3C", lwd=2, lty=2)
                                if (!is.null(lsl)) abline(h=lsl, col="#9B59B6", lwd=1.5, lty=3)
                                if (!is.null(usl)) abline(h=usl, col="#9B59B6", lwd=1.5, lty=3)
                                if (length(i_ooc)>0)
                                    points(i_ooc, dat[i_ooc], pch=21, cex=2.0,
                                           col="#E74C3C", bg="#FDEDEC", lwd=2)
                                legend("topright",
                                       legend=c(sprintf("CL = %.4f", ri$xbar),
                                                sprintf("UCL = %.4f", imrd$i_ucl),
                                                sprintf("LCL = %.4f", imrd$i_lcl),
                                                spec_lbl),
                                       col=c("#27AE60","#E74C3C","#E74C3C", spec_col),
                                       lty=c(1L,2L,2L,spec_lty), lwd=2, cex=0.78, bty="n")
                                grid(col="grey92", lty=1, lwd=0.5)
                            })

                            # ── MR-chart (separate Submit) ─────────────────────
                            mr_ooc  <- which(mr > imrd$mr_ucl)
                            ylim_mr <- c(0, max(imrd$mr_ucl, max(mr), na.rm=TRUE)*1.18)
                            spssRGraphics.Submit({
                                graphics::layout(1); par(mfrow=c(1,1))
                                op_m <- par(mar=c(4,4.5,3.5,2))
                                on.exit(par(op_m), add=TRUE)
                                plot(seq_len(n_i-1), mr, type="o",
                                     pch=19, col="#E67E22", cex=0.7, lwd=1.5,
                                     main=paste0("Moving Range (MR) Chart  —  ", ri$varlab),
                                     xlab="Observation", ylab="Moving Range",
                                     ylim=ylim_mr)
                                abline(h=imrd$mr_bar, col="#27AE60", lwd=2)
                                abline(h=imrd$mr_ucl, col="#E74C3C", lwd=2, lty=2)
                                abline(h=0,           col="grey60",  lwd=1)
                                if (length(mr_ooc)>0)
                                    points(mr_ooc, mr[mr_ooc], pch=21, cex=2.0,
                                           col="#E74C3C", bg="#FDEDEC", lwd=2)
                                legend("topright",
                                       legend=c(sprintf("MR̅ = %.4f", imrd$mr_bar),
                                                sprintf("UCL = %.4f", imrd$mr_ucl)),
                                       col=c("#27AE60","#E74C3C"), lty=c(1,2),
                                       lwd=2, cex=0.78, bty="n")
                                grid(col="grey92", lty=1, lwd=0.5)
                            })
                        }, error=function(e) warns$warn(gtxtf("I-MR chart error (%s): %s", ri$varlab, e$message)))
                    })
                }
            }

            # ─────────────────────────────────────────────────────────────
            # CHART SET 3: Violin Plot
            # ─────────────────────────────────────────────────────────────
            if (ch_violin) {
                tryCatch({
                    spssRGraphics.Submit({
                        graphics::layout(1); par(mfrow=c(1,1))
                        op <- par(mar=c(5,4.5,3,2))
                        # par reset done explicitly at block end (on.exit fires on outer fn, not here)

                        n_v   <- length(results)
                        x_pos <- seq_len(n_v)
                        ylim_v <- range(unlist(lapply(results, `[[`, "data")),
                                        lsl, usl, na.rm=TRUE)
                        pad_v <- diff(ylim_v)*0.08; ylim_v <- ylim_v+c(-pad_v, pad_v)

                        plot(c(0.4, n_v+0.6), ylim_v, type="n",
                             main="Violin Plot", xlab="",
                             ylab="Value",
                             xaxt="n", xlim=c(0.4, n_v+0.6))
                        axis(1, at=x_pos,
                             labels=sapply(results, `[[`, "varlab"),
                             las=if (n_v == 1) 1 else 2, cex.axis=0.8)

                        grid(col="grey92", lty=1, lwd=0.5)
                        for (vi in seq_along(results)) {
                            ri    <- results[[vi]]
                            col_i <- VAR_PALETTE[((vi-1)%%length(VAR_PALETTE))+1]
                            dens  <- density(ri$data, bw="nrd0")
                            ds    <- dens$y / max(dens$y) * 0.35
                            polygon(c(vi+ds, vi-rev(ds)),
                                    c(dens$x, rev(dens$x)),
                                    col=paste0(col_i,"55"), border=col_i, lwd=1.5)
                            # Jitter points to show individual data variation
                            set.seed(42L)
                            jx <- vi + runif(length(ri$data), -0.12, 0.12) * ds[
                                findInterval(ri$data, dens$x, all.inside=TRUE)] /
                                max(ds) * 0.9
                            points(jx, ri$data, pch=16, cex=0.35,
                                   col=paste0(col_i, "70"))
                            bxs <- boxplot.stats(ri$data)
                            rect(vi-0.1, bxs$stats[2], vi+0.1, bxs$stats[4],
                                 col="#FFFFFF99", border="#2C3E50", lwd=2)
                            segments(vi-0.1, bxs$stats[3], vi+0.1, bxs$stats[3],
                                     lwd=3, col="#E74C3C")
                            points(vi, ri$xbar, pch=18, cex=1.5, col=col_i)
                        }
                        draw_spec_h()
                        spec_legend()
                        par(op)
                    })
                }, error=function(e) warns$warn(gtxtf("Violin plot error: %s", e$message)))
            }

            # ─────────────────────────────────────────────────────────────
            # CHART SET 4: Kernel Density Estimation
            # ─────────────────────────────────────────────────────────────
            if (ch_kde) {
                tryCatch({
                    spssRGraphics.Submit({
                        graphics::layout(1); par(mfrow=c(1,1))
                        op <- par(mar=c(4,4.5,3,2))
                        # par reset done explicitly at block end (on.exit fires on outer fn, not here)

                        densities <- lapply(lapply(results, `[[`, "data"), density, bw="nrd0")
                        ylim_k  <- c(0, max(sapply(densities, function(d) max(d$y))) * 1.1)
                        all_x_k <- unlist(lapply(densities, `[[`, "x"))
                        xlim_k  <- range(c(all_x_k, lsl, usl), na.rm=TRUE)

                        plot(xlim_k, ylim_k, type="n",
                             main="Kernel Density Estimation",
                             xlab="Measurement", ylab="Density")

                        for (vi in seq_along(results)) {
                            ri    <- results[[vi]]
                            dens  <- densities[[vi]]
                            col_i <- VAR_PALETTE[((vi-1)%%length(VAR_PALETTE))+1]
                            polygon(c(dens$x, rev(dens$x)),
                                    c(dens$y, rep(0,length(dens$y))),
                                    col=paste0(col_i,"33"), border=NA)
                            lines(dens, col=col_i, lwd=2.5)
                            abline(v=ri$xbar, col=col_i, lwd=1.5, lty=3)
                        }
                        draw_spec_v()
                        if (multi_var)
                            legend("topright",
                                   legend=sapply(results, `[[`, "varlab"),
                                   col=VAR_PALETTE[seq_along(results)],
                                   lty=1, lwd=2, cex=0.75, bty="n")
                        else
                            spec_legend(extra_nm=c("KDE","Mean"),
                                        extra_col=c("#2980B9","#27AE60"),
                                        extra_lty=c(1L,3L))
                        grid(col="grey92", lty=1, lwd=0.5)
                        par(op)
                    })
                }, error=function(e) warns$warn(gtxtf("KDE plot error: %s", e$message)))
            }

            # ─────────────────────────────────────────────────────────────
            # CHART SET 4b: Per-Variable Run Charts (all variables in a grid)
            # ─────────────────────────────────────────────────────────────
            if (ch_run && n_vars > 0) {
                tryCatch({
                    spssRGraphics.Submit({
                        graphics::layout(1); par(mfrow=c(1,1))  # reset any prior state
                        ncols_r <- min(2L, n_vars)
                        nrows_r <- ceiling(n_vars / ncols_r)
                        op <- par(mfrow = c(nrows_r, ncols_r),
                                  mar = c(4, 4, 3, 1), oma = c(0,0,3,0),
                                  cex.lab = 0.95, cex.axis = 0.85)
                        # par reset done explicitly at block end (on.exit fires on outer fn, not here)

                        for (vi in seq_along(results)) {
                            ri    <- results[[vi]]
                            di    <- ri$data
                            ni    <- ri$n
                            xi    <- ri$xbar
                            col_i <- VAR_PALETTE[((vi-1) %% length(VAR_PALETTE)) + 1]

                            yr <- range(c(di, lsl, usl), na.rm = TRUE)
                            yr <- yr + diff(yr) * c(-0.05, 0.05)

                            plot(seq_len(ni), di, type="o",
                                 pch=19, col=col_i, cex=0.55, lwd=1.2,
                                 main=ri$varlab,
                                 xlab="Observation Order", ylab="Value",
                                 ylim=yr)
                            abline(h=xi, col="#27AE60", lwd=1.8)
                            if (!is.null(lsl)) abline(h=lsl, col="#E74C3C", lwd=1.5, lty=2)
                            if (!is.null(usl)) abline(h=usl, col="#E74C3C", lwd=1.5, lty=2)
                            if (!is.null(target)) abline(h=target, col="#27AE60", lwd=1.2, lty=3)

                            # Highlight out-of-spec points
                            ooc <- integer(0)
                            if (!is.null(lsl)) ooc <- c(ooc, which(di < lsl))
                            if (!is.null(usl)) ooc <- c(ooc, which(di > usl))
                            if (length(unique(ooc)) > 0)
                                points(unique(ooc), di[unique(ooc)],
                                       pch=21, cex=1.6, col="#E74C3C", bg="#FDEDEC", lwd=2)
                            grid(col="grey92", lty=1, lwd=0.4)
                        }
                        mtext(paste0("Run Charts — ", all_vars_lbl),
                              outer=TRUE, cex=1.1, font=2, col="#2C3E50")
                        # fill any unfilled grid cells (when n_vars doesn't divide evenly)
                        n_leftover <- nrows_r * ncols_r - n_vars
                        if (n_leftover > 0L) for (.k in seq_len(n_leftover)) plot.new()
                        par(op)
                    })
                }, error=function(e) warns$warn(gtxtf("Run chart error: %s", e$message)))
            }

            # ─────────────────────────────────────────────────────────────
            # CHART SET 4c: Process Capability Summary Report (per variable)
            #   Row 0 (header) : Logo + title + metadata bar
            #   Row 1 left     : Process Data + Normality test + Grade card
            #   Row 1 centre   : Histogram with Overall & Within curves, spec lines,
            #                    ±3σ tolerance band, KDE overlay
            #   Row 1 right top: Overall Capability (Pp/PPL/PPU/Ppk/Cpm) + gauge bars
            #   Row 1 right bot: Within Capability  (Cp/CPL/CPU/Cpk + CI) + gauge bars
            #   Row 2 (footer) : PPM | DPMO | Yield% | Sigma Level | Grade | CI on Cpk
            # ─────────────────────────────────────────────────────────────
            for (vi_cap in seq_along(results)) {
                local({
                    ri_c  <- results[[vi_cap]]
                    dat_c <- ri_c$data
                    n_c   <- ri_c$n
                    if (n_c < 3L) return(invisible(NULL))
                    xb_c  <- ri_c$xbar
                    sw_c  <- ri_c$s_within
                    so_c  <- ri_c$s
                    vlab  <- ri_c$varlab
                    cp_c  <- ri_c$cp;  cpk_c <- ri_c$cpk
                    cpl_c <- ri_c$cpl; cpu_c <- ri_c$cpu
                    pp_c  <- ri_c$pp;  ppk_c <- ri_c$ppk
                    ppl_c <- ri_c$cpl_o; ppu_c <- ri_c$cpu_o
                    cpm_c <- ri_c$cpm

                    # ── Extra metrics beyond the standard set ───────────────────────
                    # Shapiro-Wilk normality
                    sw_p  <- tryCatch({
                        swt <- shapiro.test(if (n_c > 5000) sample(dat_c,5000) else dat_c)
                        swt$p.value
                    }, error=function(e) NA_real_)
                    sw_w  <- tryCatch(shapiro.test(if (n_c>5000) sample(dat_c,5000) else dat_c)$statistic,
                                      error=function(e) NA_real_)
                    normal_ok <- !is.na(sw_p) && sw_p > 0.05

                    # PPM observed
                    obs_b <- if (!is.null(lsl)) sum(dat_c < lsl) / n_c * 1e6 else NA
                    obs_a <- if (!is.null(usl)) sum(dat_c > usl) / n_c * 1e6 else NA
                    obs_t <- if (any(!is.na(c(obs_b,obs_a)))) sum(c(obs_b,obs_a),na.rm=TRUE) else NA
                    # PPM expected overall
                    exp_b_ov <- if (!is.null(lsl)) pnorm((lsl-xb_c)/so_c)*1e6 else NA
                    exp_a_ov <- if (!is.null(usl)) (1-pnorm((usl-xb_c)/so_c))*1e6 else NA
                    exp_t_ov <- if (any(!is.na(c(exp_b_ov,exp_a_ov)))) sum(c(exp_b_ov,exp_a_ov),na.rm=TRUE) else NA
                    # PPM expected within
                    exp_b_wi <- if (!is.null(lsl)) pnorm((lsl-xb_c)/sw_c)*1e6 else NA
                    exp_a_wi <- if (!is.null(usl)) (1-pnorm((usl-xb_c)/sw_c))*1e6 else NA
                    exp_t_wi <- if (any(!is.na(c(exp_b_wi,exp_a_wi)))) sum(c(exp_b_wi,exp_a_wi),na.rm=TRUE) else NA

                    # DPMO = ppm (same unit; label it DPMO for Six Sigma context)
                    dpmo_ov <- exp_t_ov
                    dpmo_wi <- exp_t_wi

                    # Yield %
                    yield_ov <- if (!is.na(exp_t_ov)) (1 - exp_t_ov/1e6)*100 else NA
                    yield_wi <- if (!is.na(exp_t_wi)) (1 - exp_t_wi/1e6)*100 else NA

                    # Sigma level — derived from WITHIN-sigma expected PPM (matches
                    # the Cp/Cpk basis of this Sixpack panel). NOTE: this is
                    # deliberately the WITHIN-based Z-bench, distinct from the
                    # OVERALL-based "Z-Bench (Long-Term)" shown in the main
                    # per-variable Z-Bench/Benchmark Sigma tables — the two use
                    # different sigma estimators and will not numerically match.
                    dpo_wi   <- if (!is.na(exp_t_wi)) exp_t_wi/1e6 else NA
                    z_lt     <- if (!is.na(dpo_wi) && dpo_wi > 0 && dpo_wi < 1)
                                    -qnorm(dpo_wi) else NA
                    z_st     <- if (!is.na(z_lt)) z_lt + 1.5 else NA   # 1.5-sigma drift

                    # CI on Cpk (approximate, Bissell 1990)
                    cpk_ci_lo <- NA; cpk_ci_hi <- NA
                    if (!is.null(cpk_c) && !is.na(cpk_c) && n_c >= 5 &&
                        !is.null(conf)) {
                        alpha  <- 1 - conf
                        z_a2   <- qnorm(1 - alpha/2)
                        se_cpk <- cpk_c * sqrt(1/(9*n_c*cpk_c^2) + 1/(2*(n_c-1)))
                        cpk_ci_lo <- cpk_c - z_a2 * se_cpk
                        cpk_ci_hi <- cpk_c + z_a2 * se_cpk
                    }

                    # Grade (based on Cpk, or Ppk if no within SD)
                    grade_val <- if (!is.null(cpk_c) && !is.na(cpk_c)) cpk_c
                                 else if (!is.null(ppk_c) && !is.na(ppk_c)) ppk_c
                                 else NA
                    grade_lbl <- if (is.na(grade_val))          "Ungraded"
                                 else if (grade_val >= 1.67)    "Excellent"
                                 else if (grade_val >= 1.33)    "Capable"
                                 else if (grade_val >= 1.00)    "Marginal"
                                 else                           "Not Capable"
                    grade_col <- switch(grade_lbl,
                                        "Excellent"    = "#27AE60",
                                        "Capable"      = "#2980B9",
                                        "Marginal"     = "#E67E22",
                                        "Not Capable"  = "#E74C3C",
                                        "#888888")

                    # ── Cap-gauge helper (draw a mini horizontal bar) ──────
                    draw_gauge <- function(val, x0, x1, y, h=0.035,
                                          lo=0, hi=2, thresh=1.33) {
                        if (is.null(val) || is.na(val)) return(invisible(NULL))
                        col_g <- if (val>=1.67) "#27AE60" else if (val>=1.33) "#2980B9"
                                 else if (val>=1.00) "#E67E22" else "#E74C3C"
                        frac  <- min(1, max(0, (val-lo)/(hi-lo)))
                        rect(x0, y-h/2, x0+frac*(x1-x0), y+h/2,
                             col=col_g, border=NA)
                        rect(x0, y-h/2, x1, y+h/2, col=NA, border="#AAAAAA", lwd=0.6)
                        abline(v=x0+(thresh-lo)/(hi-lo)*(x1-x0),
                               col="#888888", lty=2, lwd=0.6)
                    }

                    fmtppm  <- function(v) if (!is.null(v) && !is.na(v)) formatC(v,1,format="f") else "—"
                    fmtf2   <- function(v) if (!is.null(v) && !is.na(v)) formatC(v,2,format="f") else "—"
                    fmtf4   <- function(v) if (!is.null(v) && !is.na(v)) formatC(v,4,format="f") else "—"
                    fmtpct  <- function(v) if (!is.null(v) && !is.na(v)) sprintf("%.4f%%", v) else "—"

                    tryCatch({
                        spssRGraphics.Submit({
                            # Layout matrix:
                            #  1 1 1 1   ← header bar
                            #  2 3 3 4   ← process data | histogram | cap overall
                            #  2 3 3 5   ← process data | histogram | cap within
                            #  6 6 6 6   ← extended footer table
                            graphics::layout(1); par(mfrow=c(1,1))  # reset any prior state first
                            graphics::layout(matrix(c(1,1,1,1,
                                            2,3,3,4,
                                            2,3,3,5,
                                            6,6,6,6), 4, 4, byrow=TRUE),
                                   widths  = c(2.2, 2.4, 2.4, 2.2),
                                   heights = c(0.7, 2.6, 2.0, 1.6))

                            # ── Panel 1: Header ────────────────────────────────
                            par(mar=c(0,0,0,0))
                            plot.new(); plot.window(xlim=c(0,1), ylim=c(0,1))
                            rect(0,0,1,1, col="#2C3E50", border=NA)
                            title_txt <- paste0("Process Capability Analysis  —  ", vlab)
                            text(0.5, 0.70, title_txt,
                                 col="white", font=2, cex=1.10, adj=c(0.5,0.5))
                            meta_parts <- c(
                                if (!is.null(preparedby) && nchar(trimws(preparedby))>0)
                                    paste0("Prepared by: ", trimws(preparedby)) else NULL,
                                paste0("n = ", n_c),
                                format(Sys.Date(), "Date: %d %b %Y"))
                            text(0.5, 0.22, paste(meta_parts, collapse="   |   "),
                                 col="#BDC3C7", cex=0.75, adj=c(0.5,0.5))
                            # Grade badge (right side)
                            rect(0.76, 0.12, 0.99, 0.88, col=grade_col, border=NA)
                            text(0.875, 0.55, grade_lbl,
                                 col="white", font=2, cex=0.88, adj=c(0.5,0.5))
                            # Logo (left side)
                            if (!is.null(logo_img)) {
                                old_usr <- par("usr")
                                add_logo_to_plot(logo_img, "topleft", size=0.28)
                            }

                            # ── Panel 2: Process Data + Normality ─────────────
                            par(mar=c(0.3,0.3,0.3,0.3))
                            plot.new(); plot.window(xlim=c(0,1), ylim=c(0,1))
                            rect(0,0,1,1, col="#F8F9FA", border="#CED4DA", lwd=1.2)

                            # Section: Process Data
                            rect(0,0.60,1,1, col="#EBF5FB", border=NA)
                            text(0.5, 0.97, "Process Data", font=2, cex=0.82,
                                 adj=c(0.5,1), col="#1A5276")
                            abline(h=0.92, col="#AED6F1", lwd=1.0)
                            pd_lbl <- c("LSL","Target","USL","Mean","N",
                                        "SD (Overall)","SD (Within)")
                            pd_val <- c(fmtf4(lsl), fmtf4(target), fmtf4(usl),
                                        fmtf4(xb_c), as.character(n_c),
                                        formatC(so_c,5,format="f"),
                                        formatC(sw_c,5,format="f"))
                            for (pi in seq_along(pd_lbl)) {
                                yp <- 0.89 - (pi-1)*0.044
                                text(0.04, yp, pd_lbl[pi], cex=0.68, adj=c(0,0.5), col="#2C3E50")
                                text(0.96, yp, pd_val[pi],  cex=0.68, adj=c(1,0.5), col="#555555")
                            }

                            # Section: Normality Test
                            rect(0,0.30,1,0.60, col="#FDFEFE", border=NA)
                            text(0.5, 0.595, "Normality (Shapiro-Wilk)", font=2, cex=0.78,
                                 adj=c(0.5,1), col="#1A5276")
                            abline(h=0.555, col="#D5DBDB", lwd=0.8)
                            if (!is.na(sw_w)) {
                                text(0.04, 0.525, "W statistic", cex=0.68, adj=c(0,0.5))
                                text(0.96, 0.525, formatC(as.numeric(sw_w),4,format="f"),
                                     cex=0.68, adj=c(1,0.5))
                                text(0.04, 0.487, "p-value",    cex=0.68, adj=c(0,0.5))
                                text(0.96, 0.487, sprintf("%.4f", sw_p),
                                     cex=0.68, adj=c(1,0.5),
                                     col=if(normal_ok) "#27AE60" else "#E74C3C")
                                text(0.04, 0.450, "Decision",   cex=0.68, adj=c(0,0.5))
                                text(0.96, 0.450,
                                     if(normal_ok) "Normal (p>0.05)" else "Non-normal",
                                     cex=0.68, adj=c(1,0.5),
                                     col=if(normal_ok) "#27AE60" else "#E74C3C", font=2)
                            } else {
                                text(0.5, 0.490, "Not available", cex=0.68,
                                     adj=c(0.5,0.5), col="#888888")
                            }

                            # Section: Grade Card
                            rect(0,0,1,0.30, col=grade_col, border=NA)
                            text(0.5, 0.27, "Process Grade", cex=0.72, adj=c(0.5,0.5),
                                 col="white", font=2)
                            text(0.5, 0.16, grade_lbl, cex=1.00, adj=c(0.5,0.5),
                                 col="white", font=2)
                            abline(h=0.065, col=adjustcolor("white",0.35), lwd=0.6)
                            text(0.5, 0.033,
                                 "≥1.67 Excellent  ≥1.33 Capable  ≥1.00 Marginal  <1.00 Not Capable",
                                 cex=0.50, adj=c(0.5,0.5),
                                 col=adjustcolor("white",0.80))

                            # ── Panel 3: Histogram ─────────────────────────────
                            par(mar=c(3.2,3.2,2.0,0.8))
                            xlim_c <- range(c(dat_c, lsl, usl), na.rm=TRUE)
                            pad    <- diff(xlim_c) * 0.12
                            xlim_c <- c(xlim_c[1]-pad, xlim_c[2]+pad)
                            nbr_c  <- max(8L, min(30L, ceiling(sqrt(n_c)*1.5)))

                            h_obj <- hist(dat_c, breaks=nbr_c, plot=FALSE)
                            ylim_h <- c(0, max(h_obj$density)*1.45)

                            # ±3σ within tolerance band (shaded)
                            mu3lo <- xb_c - 3*sw_c; mu3hi <- xb_c + 3*sw_c
                            plot(h_obj, freq=FALSE, col="#AED6F1", border="#FFFFFF",
                                 main="", xlab="", ylab="Density",
                                 xlim=xlim_c, ylim=ylim_h,
                                 cex.axis=0.78, cex.lab=0.82)
                            title(main=paste0("Capability Report — ", vlab),
                                  cex.main=0.88, font.main=2, col.main="#2C3E50")

                            # ±3σ within band
                            rect(max(xlim_c[1],mu3lo), 0, min(xlim_c[2],mu3hi), ylim_h[2],
                                 col=adjustcolor("#27AE60",alpha.f=0.07), border=NA)

                            xseq_c <- seq(xlim_c[1], xlim_c[2], length.out=600)
                            # Overall curve — dashed
                            lines(xseq_c, dnorm(xseq_c, xb_c, so_c),
                                  col="#E74C3C", lwd=2.2, lty=2)
                            # Within curve — solid
                            lines(xseq_c, dnorm(xseq_c, xb_c, sw_c),
                                  col="#2C3E50", lwd=2.2, lty=1)

                            if (!is.null(lsl)) {
                                abline(v=lsl, col="#E74C3C", lwd=2.0, lty=1)
                                mtext("LSL", side=3, at=lsl, cex=0.72,
                                      col="#E74C3C", line=0.2)
                            }
                            if (!is.null(usl)) {
                                abline(v=usl, col="#E74C3C", lwd=2.0, lty=1)
                                mtext("USL", side=3, at=usl, cex=0.72,
                                      col="#E74C3C", line=0.2)
                            }
                            if (!is.null(target)) {
                                abline(v=target, col="#27AE60", lwd=1.6, lty=3)
                                mtext("TGT", side=3, at=target, cex=0.68,
                                      col="#27AE60", line=0.2)
                            }

                            legend("topright",
                                   legend = c("Within (Cp/Cpk)","Overall (Pp/Ppk)",
                                              "Spec limits","±3σ Within"),
                                   lty    = c(1,2,1,NA),
                                   pch    = c(NA,NA,NA,15),
                                   col    = c("#2C3E50","#E74C3C","#E74C3C",
                                              adjustcolor("#27AE60",0.35)),
                                   lwd    = c(2,2,2,NA),
                                   pt.cex = 1.8,
                                   cex    = 0.72, bty="o",
                                   bg     = adjustcolor("white",0.85))
                            grid(col="grey92", lty=1, lwd=0.4)

                            # ── Panel 4: Overall Capability ────────────────────
                            par(mar=c(0.3,0.3,0.3,0.3))
                            plot.new(); plot.window(xlim=c(0,1), ylim=c(0,1))
                            rect(0,0,1,1, col="#F8F9FA", border="#CED4DA", lwd=1.2)
                            rect(0,0.88,1,1, col="#EBF5FB", border=NA)
                            text(0.5, 0.975, "Overall Capability", font=2, cex=0.82,
                                 adj=c(0.5,0.5), col="#1A5276")
                            abline(h=0.88, col="#AED6F1", lwd=1.0)

                            ov_entries <- list(
                                list("Pp",   pp_c),  list("PPL",  ppl_c),
                                list("PPU",  ppu_c), list("Ppk",  ppk_c),
                                list("Cpm",  cpm_c))
                            for (oi in seq_along(ov_entries)) {
                                lbl_o <- ov_entries[[oi]][[1]]
                                val_o <- ov_entries[[oi]][[2]]
                                yc    <- 0.83 - (oi-1)*0.155
                                col_o <- if (!is.null(val_o) && !is.na(val_o)) {
                                    if (val_o>=1.67) "#27AE60" else if (val_o>=1.33) "#2980B9"
                                    else if (val_o>=1.00) "#E67E22" else "#E74C3C"
                                } else "#888888"
                                text(0.05, yc+0.020, lbl_o, cex=0.74, adj=c(0,0.5), col="#333333")
                                text(0.95, yc+0.020, fmtf2(val_o), cex=0.74,
                                     adj=c(1,0.5), font=2, col=col_o)
                                draw_gauge(val_o, 0.05, 0.95, yc-0.022, h=0.028)
                            }

                            # ── Panel 5: Within (Potential) Capability ─────────
                            plot.new(); plot.window(xlim=c(0,1), ylim=c(0,1))
                            rect(0,0,1,1, col="#F8F9FA", border="#CED4DA", lwd=1.2)
                            rect(0,0.88,1,1, col="#EAF7F0", border=NA)
                            text(0.5, 0.975, "Within (Potential) Cap.", font=2, cex=0.82,
                                 adj=c(0.5,0.5), col="#1A5276")
                            abline(h=0.88, col="#A9DFBF", lwd=1.0)

                            wi_entries <- list(
                                list("Cp",  cp_c),  list("CPL", cpl_c),
                                list("CPU", cpu_c), list("Cpk", cpk_c))
                            for (wi in seq_along(wi_entries)) {
                                lbl_w <- wi_entries[[wi]][[1]]
                                val_w <- wi_entries[[wi]][[2]]
                                yc2   <- 0.83 - (wi-1)*0.165
                                col_w <- if (!is.null(val_w) && !is.na(val_w)) {
                                    if (val_w>=1.67) "#27AE60" else if (val_w>=1.33) "#2980B9"
                                    else if (val_w>=1.00) "#E67E22" else "#E74C3C"
                                } else "#888888"
                                text(0.05, yc2+0.022, lbl_w, cex=0.74, adj=c(0,0.5), col="#333333")
                                text(0.95, yc2+0.022, fmtf2(val_w), cex=0.74,
                                     adj=c(1,0.5), font=2, col=col_w)
                                draw_gauge(val_w, 0.05, 0.95, yc2-0.020, h=0.028)
                            }
                            # Cpk CI (extra info typically shown only in a table)
                            if (!is.na(cpk_ci_lo)) {
                                ci_pct <- if (!is.null(conf)) sprintf("%.0f%%", conf*100) else "95%"
                                text(0.5, 0.12,
                                     sprintf("Cpk %s CI: [%s, %s]",
                                             ci_pct, fmtf2(cpk_ci_lo), fmtf2(cpk_ci_hi)),
                                     cex=0.68, adj=c(0.5,0.5), col="#555555")
                            }

                            # ── Panel 6: Extended footer table ─────────────────
                            par(mar=c(0.2,0.2,0.2,0.2))
                            plot.new(); plot.window(xlim=c(0,1), ylim=c(0,1))
                            rect(0,0,1,1, col="#2C3E50", border=NA)

                            # Column headers
                            cols_x <- c(0.06, 0.20, 0.34, 0.48, 0.62, 0.76, 0.90)
                            hdrs   <- c("Metric","Observed","Exp. (Overall)","Exp. (Within)",
                                        "DPMO","Yield %","Sigma Level (Within)")
                            for (ci2 in seq_along(hdrs)) {
                                text(cols_x[ci2], 0.84, hdrs[ci2],
                                     cex=0.68, adj=c(0,0.5), col="#AED6F1", font=2)
                            }
                            abline(h=0.72, col="#566573", lwd=0.8)

                            rows_data <- list(
                                list("PPM < LSL",
                                     fmtppm(obs_b), fmtppm(exp_b_ov), fmtppm(exp_b_wi),
                                     "—", "—", "—"),
                                list("PPM > USL",
                                     fmtppm(obs_a), fmtppm(exp_a_ov), fmtppm(exp_a_wi),
                                     "—", "—", "—"),
                                list("PPM Total",
                                     fmtppm(obs_t), fmtppm(exp_t_ov), fmtppm(exp_t_wi),
                                     fmtppm(dpmo_ov),
                                     fmtpct(yield_wi),
                                     if (!is.na(z_st)) sprintf("Z_st(wi)=%.2f Z_lt(wi)=%.2f",
                                                                z_st, z_lt) else "—"))
                            for (ri2 in seq_along(rows_data)) {
                                yp5 <- 0.62 - (ri2-1)*0.22
                                row_bg <- if (ri2==3) "#34495E" else NA
                                if (!is.na(row_bg)) rect(0.01,yp5-0.09,0.99,yp5+0.12,
                                                          col=row_bg,border=NA)
                                for (ci3 in seq_along(rows_data[[ri2]])) {
                                    text(cols_x[ci3], yp5, rows_data[[ri2]][[ci3]],
                                         cex=0.67, adj=c(0,0.5),
                                         col=if(ri2==3) "#F0F3F4" else "#BDC3C7",
                                         font=if(ri2==3) 2 else 1)
                                }
                            }
                            graphics::layout(1)    # reset layout before returning device
                        })
                    }, error=function(e) warns$warn(gtxtf("Capability report chart error (%s): %s",
                                                           vlab, e$message)))
                })
            }

            # CHART SET 5: Capability Sixpack (3×2)
            # ─────────────────────────────────────────────────────────────
            if (ch_sixpack) {
                tryCatch({
                    spssRGraphics.Submit({
                        op <- par(mfrow=c(3,2), mar=c(3.5,3.5,2.5,1.5),
                                  oma=c(0,0,3,0), cex.lab=0.95, cex.axis=0.85)
                        # par reset done explicitly at block end (on.exit fires on outer fn, not here)

                        nbr6  <- max(5L, ceiling(sqrt(n1)))
                        xlim6 <- range(c(data1, lsl, usl), na.rm=TRUE)
                        pad6  <- diff(xlim6)*0.08; xlim6 <- xlim6+c(-pad6,pad6)

                        panel6_safe <- function(expr) {
                            tryCatch(expr, error = function(e) {
                                tryCatch({
                                    plot.new(); plot.window(xlim=c(0,1), ylim=c(0,1))
                                    text(0.5, 0.5, paste0("Panel error:\n", conditionMessage(e)),
                                         col="#E74C3C", cex=0.6)
                                }, error = function(e2) invisible(NULL))
                            })
                        }

                        # 1 — Run Chart
                        panel6_safe({
                            plot(seq_len(n1), data1, type="o", pch=19, col="#2980B9",
                                 cex=0.5, lwd=1.2, main="Run Chart", xlab="Obs", ylab="Value")
                            abline(h=xbar1, col="#27AE60", lwd=1.5); draw_spec_h()
                            grid(col="grey92", lty=1, lwd=0.4)
                        })

                        # 2 — Histogram
                        panel6_safe({
                            hist(data1, breaks=nbr6, freq=FALSE, col="#AED6F1", border="white",
                                 main="Histogram", xlab="Value", ylab="Density", xlim=xlim6)
                            xseq6 <- seq(xlim6[1], xlim6[2], length.out=200)
                            lines(xseq6, dnorm(xseq6, xbar1, s1), col="#2980B9", lwd=2)
                            draw_spec_v()
                        })

                        # 3 — Normal Q-Q
                        panel6_safe({
                            qqnorm(data1, main="Normal Q-Q", pch=19, col="#2980B9", cex=0.55)
                            qqline(data1, col="#E74C3C", lwd=2); grid(col="grey92",lty=1,lwd=0.4)
                        })

                        # 4 — Capability Distribution
                        panel6_safe({
                            span6 <- max(4.5*s1,
                                         if (!is.null(lsl)) abs(xbar1-lsl)+s1 else 0,
                                         if (!is.null(usl)) abs(usl-xbar1)+s1 else 0)
                            xs6   <- seq(xbar1-span6, xbar1+span6, length.out=250)
                            ys6   <- dnorm(xs6, xbar1, s1)
                            xlim_c6 <- range(c(xs6, lsl, usl), na.rm=TRUE)
                            plot(xs6, ys6, type="l", col="#2980B9", lwd=2,
                                 main="Capability", xlab="Value", ylab="Density",
                                 xlim=xlim_c6)
                            polygon(c(xs6,rev(xs6)),c(ys6,rep(0,length(ys6))),
                                    col="#D6EAF8", border=NA)
                            lines(xs6, ys6, col="#2980B9", lwd=2)
                            draw_spec_v()
                            if (!is.na(cpk1))
                                legend("topright", legend=sprintf("Cpk = %.3f",cpk1),
                                       bty="o", bg="#FFFFF0", cex=0.8, box.lwd=0.5)
                        })

                        # 5 — Box Plot
                        panel6_safe({
                            boxplot(data1, col="#AED6F1", border="#2C3E50",
                                    main="Box Plot", ylab="Value",
                                    pch=19, outcex=0.7, outcol="#E74C3C")
                            draw_spec_h(); grid(col="grey92",lty=1,lwd=0.4)
                        })

                        # 6 — Statistics panel
                        panel6_safe({
                            plot.new(); plot.window(xlim=c(0,1), ylim=c(0,1))
                            title(main="Indices", font.main=2, cex.main=0.95)
                            rect(0,0,1,1,col="#FAFAFA",border="#BDC3C7")
                            sp_lines <- c(
                                sprintf("n   = %d",    n1),
                                sprintf("Mean= %.4f",  xbar1),
                                sprintf("s   = %.4f",  s1),
                                "",
                                paste0("Cp  = ", safe_idx("", cp1)),
                                paste0("Cpk = ", safe_idx("", cpk1)),
                                paste0("Pp  = ", safe_idx("", pp1)),
                                paste0("Ppk = ", safe_idx("", ppk1)),
                                paste0("Cpm = ", safe_idx("", cpm1)),
                                "",
                                sprintf("Out = %.4f%%", r1$pct_out),
                                paste0("PPM = ", safe_idx("", ppt1, fmt="%.1f"))
                            )
                            text(0.5, 0.95, paste(sp_lines, collapse="\n"),
                                 cex=0.78, adj=c(0.5,1), col="#2C3E50")
                        })

                        # Logo + outer title — isolated so a raster/file issue here
                        # cannot abort the whole Sixpack chart (incl. the Box Plot panel)
                        tryCatch(add_logo_to_plot(logo_img, "topright", size=0.10),
                                 error = function(e) invisible(NULL))
                        tryCatch(mtext(paste0("Capability Sixpack — ", first_varlab),
                                       outer=TRUE, cex=1.1, font=2, col="#2C3E50"),
                                 error = function(e) invisible(NULL))
                        par(op)
                    })
                }, error=function(e) {
                    warns$warn(gtxtf("Sixpack chart error: %s", e$message))
                    # Re-emit as visible message too
                    message("SIXPACK ERROR: ", e$message)
                })
            }

            # ─────────────────────────────────────────────────────────────
            # HTML REPORT: Plotly interactive charts (responsive, zoomable)
            # ─────────────────────────────────────────────────────────────
            tryCatch({
                has_plotly <- tryCatch({
                    .spss_ensure_packages(c("plotly", "jsonlite", "htmlwidgets", "base64enc"))
                    requireNamespace("plotly",  quietly=TRUE) &&
                    requireNamespace("jsonlite", quietly=TRUE)
                }, error=function(e) FALSE)

                if (!has_plotly) {
                    warns$warn("plotly/jsonlite not available — HTML report skipped. Install with: install.packages('plotly')")
                } else {
                    suppressWarnings(library(plotly))
                    suppressWarnings(library(jsonlite))
                    options(plotly.validateRec = FALSE)   # suppress attribute warnings

                    # Plotly emits noisy "marker.*" warnings (e.g. unsupported marker
                    # attributes on certain trace types) while building/validating these
                    # charts. They are cosmetic and do not affect the rendered output, so
                    # mute only warnings whose message mentions "marker"; everything else
                    # still surfaces normally.
                    .muffle_marker_warning <- function(w) {
                        if (grepl("marker", conditionMessage(w), ignore.case = TRUE)) {
                            invokeRestart("muffleWarning")
                        }
                    }
                    # Some plotly/htmlwidgets builds surface the same notices as
                    # `message()` conditions rather than `warning()` conditions —
                    # muffle those too so nothing "marker"-related leaks through.
                    .muffle_marker_message <- function(m) {
                        if (grepl("marker", conditionMessage(m), ignore.case = TRUE)) {
                            invokeRestart("muffleMessage")
                        }
                    }
                    withCallingHandlers({

                    plt_divs <- character(0)   # accumulate chart HTML

                    # ── Logo: encode to base64 NOW so all charts can use it ───
                    logo_b64_src <- ""
                    if (!is.null(logo_img) && requireNamespace("base64enc", quietly=TRUE)) {
                        tryCatch({
                            tmpf_lb <- tempfile(fileext=".png")
                            png(tmpf_lb, width=240, height=80, bg="transparent")
                            tryCatch({
                                par(mar=c(0,0,0,0)); plot.new()
                                ih <- if(is.array(logo_img)) dim(logo_img)[1] else nrow(logo_img)
                                iw <- if(is.array(logo_img)) dim(logo_img)[2] else ncol(logo_img)
                                rasterImage(logo_img,0,0,1,1)
                            }, error=function(e) NULL)
                            dev.off()
                            logo_b64_src <- paste0("data:image/png;base64,",
                                base64enc::base64encode(tmpf_lb))
                            tryCatch(file.remove(tmpf_lb), error=function(e) NULL)
                        }, error=function(e) invisible(NULL))
                    }
                    # Helper: plotly images list for logo (top-right of every chart)
                    # sizey=0.075 keeps logo within even a 60px top margin at 800px height
                    # NOTE: yanchor="top" (not "bottom") so the image's TOP edge sits at
                    # paper y=1.0 and extends DOWNWARD into the visible canvas. With
                    # yanchor="bottom" the image extended upward past y=1 and was clipped
                    # off-canvas for subplot-based figures (Run Charts / I-MR), which is
                    # why the logo was missing there even though it showed on simple charts.
                    # sizing="contain" (the default) preserves the logo's aspect ratio so
                    # it never looks squashed/stretched — "stretch" caused the logo to
                    # render distorted/inconsistent across charts of different shapes
                    # (e.g. wide Q-Q plot vs tall Run Charts). sizex/sizey below give a
                    # legible, proportionate size that matches the look of the Run
                    # Chart / I-MR overlay logo.
                    plt_logo <- if (nchar(logo_b64_src) > 0) list(list(
                        source=logo_b64_src,
                        xref="paper", yref="paper",
                        x=1.0, y=1.0,
                        sizex=0.16, sizey=0.11,
                        xanchor="right", yanchor="top",
                        sizing="contain",
                        layer="above"
                    )) else list()

                    # ── Helper: plotly object → embedded div ─────────────────
                    plt_div <- function(p, div_id, height=430, title="", is_subplot=FALSE, has_ctrl_limits=FALSE, has_spec_limits=FALSE) {
                        tryCatch({
                            # Bake the logo into the figure's layout (as a Plotly image, not an
                            # HTML overlay) so it is included when the user downloads the chart
                            # via the camera/"Download plot as png" modebar button.
                            # NOTE: plotly::subplot()-composed figures rebuild their layout at
                            # build/json time and DROP manually-attached layout$images, so the
                            # baked-in logo never appears for Run Charts / I-MR charts. For those
                            # (is_subplot=TRUE) we skip the (ineffective) embedding and instead
                            # overlay an <img> on top of the chart div — it won't be captured by
                            # the camera/download button, but it will always be visible on screen.
                            if (length(plt_logo) && !is_subplot) p <- plotly::layout(p, images = plt_logo)
                            fig_json <- suppressMessages(suppressWarnings(plotly::plotly_json(p, FALSE)))
                            cfg <- paste0('{"responsive":true,"displayModeBar":true,',
                                          '"displaylogo":false,',
                                          '"modeBarButtonsToRemove":["lasso2d","select2d"],',
                                          '"toImageButtonOptions":{"format":"png","filename":"cap_chart","scale":2}}')
                            panel_id <- paste0("thm_", div_id)
                            # Build theme swatches
                            sw <- function(nm, label, c1, c2) sprintf(
                                '<div title="%s" onclick="applyTheme(\'%s\',\'%s\')" style="background:linear-gradient(135deg,%s 50%%,%s 50%%);width:26px;height:26px;border-radius:4px;cursor:pointer;border:2px solid transparent;flex-shrink:0" onmouseover="this.style.borderColor=\'#333\'" onmouseout="this.style.borderColor=\'transparent\'"></div>',
                                label, div_id, nm, c1, c2)
                            swatches <- paste0(
                                sw("blue","Ocean Blue","#AED6F1","#2C3E50"),
                                sw("teal","Forest",    "#A8E6CF","#006064"),
                                sw("warm","Warm",      "#FFCCBC","#E65100"),
                                sw("dark","Dark",      "#546E7A","#1A237E"),
                                sw("gray","Grayscale", "#BDBDBD","#424242"))
                            theme_panel <- sprintf(
                                '<div id="%s" style="display:none;position:absolute;top:100%%;left:0;z-index:50;background:white;border:1px solid #ccc;border-radius:6px;padding:10px 12px;box-shadow:0 4px 14px rgba(0,0,0,0.18);min-width:188px;margin-top:4px">
<div style="font-size:11px;font-weight:bold;color:#555;margin-bottom:8px;letter-spacing:.5px">COLOR THEME</div>
<div style="display:flex;gap:7px;align-items:center">%s</div>
<div style="font-size:10px;color:#999;margin-top:7px">Blue &bull; Forest &bull; Warm &bull; Dark &bull; Gray</div>
</div>', panel_id, swatches)
                            # Optional "UCL/LCL" toggle button — shows/hides the
                            # statistically-calculated control-limit lines (tagged
                            # name="ctrl_limit" in shapes/annotations) without touching
                            # the always-visible center-line / spec-limit references.
                            ctrl_btn_id <- paste0("ctrlbtn_", div_id)
                            ctrl_toggle_html <- if (has_ctrl_limits) sprintf(
                                '<button id="%s" onclick="toggleCtrlLimits(\'%s\',\'%s\')" title="Show/hide statistically-calculated control limits (UCL/LCL)" style="background:#3498DB;color:white;border:1px solid #ccc;border-radius:4px;height:26px;padding:0 9px;cursor:pointer;font-size:10px;font-weight:bold;margin-left:8px;vertical-align:middle">UCL/LCL</button>',
                                ctrl_btn_id, div_id, ctrl_btn_id) else ""
                            spec_btn_id <- paste0("specbtn_", div_id)
                            spec_toggle_html <- if (has_spec_limits) sprintf(
                                '<button id="%s" onclick="toggleSpecLimits(\'%s\',\'%s\')" title="Show/hide specification limits (LSL/USL)" style="background:#8E44AD;color:white;border:1px solid #ccc;border-radius:4px;height:26px;padding:0 9px;cursor:pointer;font-size:10px;font-weight:bold;margin-left:8px;vertical-align:middle">LSL/USL</button>',
                                spec_btn_id, div_id, spec_btn_id) else
                            sprintf('<button id="%s" title="No specification limits (LSL/USL) were entered" disabled style="background:rgba(142,68,173,0.25);color:#555;border:1px solid #ccc;border-radius:4px;height:26px;padding:0 9px;cursor:not-allowed;font-size:10px;font-weight:bold;margin-left:8px;vertical-align:middle;opacity:0.55">LSL/USL</button>',
                                spec_btn_id)
                            # Gear button sits in the TITLE BAR (outside chart area — avoids squishing subplots)
                            gear_html <- sprintf(
                                '<div style="position:relative;display:inline-block;vertical-align:middle">
<button onclick="toggleThemePanel(event,\'%s\')" title="Color theme" style="background:rgba(255,255,255,0.92);border:1px solid #ccc;border-radius:4px;width:26px;height:26px;cursor:pointer;font-size:14px;line-height:1;padding:0;margin-left:8px;vertical-align:middle">&#9881;</button>%s</div>%s',
                                panel_id, theme_panel, paste0(ctrl_toggle_html, spec_toggle_html))
                            # Title bar contains heading text + gear on right
                            hdr <- if (nchar(title) > 0)
                                sprintf('<div style="display:flex;align-items:center;justify-content:space-between;margin:28px 0 6px 0">
<h3 style="color:#2C3E50;margin:0;font-size:15px;border-left:4px solid #3498DB;padding-left:10px;flex:1">%s</h3>%s</div>',
                                        title, gear_html)
                            else
                                sprintf('<div style="text-align:right;margin:4px 0 2px 0">%s</div>', gear_html)
                            # NOTE: for normal (non-subplot) figures the logo is baked into the
                            # figure's layout.images (see above) so it survives "download as
                            # png" — no separate HTML overlay needed (avoids a duplicated logo).
                            # For subplot-composed figures (is_subplot=TRUE) we add a visible
                            # HTML overlay below, since baked-in images get dropped for those.
                            # width/height set explicitly (not max-width/max-height) with
                            # object-fit:fill so we can grow it vertically without also
                            # widening it — was rendering oversized/wide on I-MR charts.
                            overlay_logo <- if (is_subplot && nchar(logo_b64_src) > 0)
                                sprintf('<img src="%s" style="position:absolute;top:6px;right:8px;width:8%%;height:13%%;object-fit:fill;z-index:5;pointer-events:none" />',
                                        logo_b64_src)
                            else ""
                            chart_wrap <- sprintf(
                                '<div style="position:relative"><div id="%s" style="width:100%%;height:%dpx"></div>%s</div>\n',
                                div_id, height, overlay_logo)
                            paste0(hdr, chart_wrap,
                                   sprintf('<script>Plotly.react("%s",%s,{},%s)</script>',
                                           div_id, fig_json, cfg))
                        }, error = function(e) {
                            sprintf('<div style="color:#c0392b;padding:10px;border:1px solid #e74c3c;border-radius:4px;margin:8px 0;font-family:monospace"><b>[Chart render error — %s]</b><br>%s</div>',
                                div_id, gsub("&","&amp;",gsub("<","&lt;",e$message)))
                        })
                    }

                    PLT_COLS <- c("#2980B9","#E74C3C","#27AE60","#8E44AD",
                                  "#F39C12","#16A085","#D35400","#2C3E50")
                    pcol <- function(vi) PLT_COLS[((vi-1L) %% length(PLT_COLS)) + 1L]

                    # ── Chart A: Capability Overview — CSS grid of 6 independent plots ──
                    # Each panel is a standalone plotly figure; CSS grid arranges them 2×3.
                    # This bypasses subplot() and manual axis domains entirely.
                    tryCatch({
                        xl_ai <- range(c(unlist(lapply(results,`[[`,"data")),lsl,usl),na.rm=TRUE)
                        xl_ai <- xl_ai + diff(xl_ai)*0.18*c(-1,1)
                        fv_ai <- function(v,d=4)
                            if(!is.null(v)&&length(v)==1&&!is.na(v)) formatC(v,d,format="f") else "—"
                        ai_lyt <- function(ttl) list(
                            paper_bgcolor="white", plot_bgcolor="#FAFAFA",
                            margin=list(l=48,r=8,t=40,b=42),
                            title=list(text=paste0("<b>",ttl,"</b>"),
                                       font=list(size=11,color="#2C3E50"),x=0.5),
                            showlegend=FALSE)

                        # ── A1: Density overlay (all vars) ───────────────────
                        yl_den <- c(0, max(sapply(results, function(ri)
                            max(dnorm(seq(xl_ai[1],xl_ai[2],l=80),ri$xbar,ri$s))))*1.35)
                        pa1 <- plot_ly()
                        for (vi in seq_along(results)) {
                            ri <- results[[vi]]; xk <- seq(xl_ai[1],xl_ai[2],l=200)
                            pa1 <- add_trace(pa1,x=xk,y=dnorm(xk,ri$xbar,ri$s),
                                type="scatter",mode="lines",name=ri$varlab,showlegend=TRUE,
                                line=list(color=pcol(vi),width=2.5))
                            pa1 <- add_trace(pa1,x=xk,y=dnorm(xk,ri$xbar,ri$s),
                                type="scatter",mode="none",showlegend=FALSE,
                                fill="tozeroy",fillcolor=paste0(substr(pcol(vi),1,7),"22"))
                        }
                        if (!is.null(lsl)) pa1 <- add_trace(pa1,x=c(lsl,lsl),y=yl_den,
                            type="scatter",mode="lines",showlegend=FALSE,hoverinfo="skip",
                            line=list(color="#E74C3C",width=1.8,dash="dash"))
                        if (!is.null(usl)) pa1 <- add_trace(pa1,x=c(usl,usl),y=yl_den,
                            type="scatter",mode="lines",showlegend=FALSE,hoverinfo="skip",
                            line=list(color="#E74C3C",width=1.8,dash="dash"))
                        if (!is.null(target)) pa1 <- add_trace(pa1,x=c(target,target),y=yl_den,
                            type="scatter",mode="lines",showlegend=FALSE,hoverinfo="skip",
                            line=list(color="#27AE60",width=1.2,dash="dot"))
                        pa1 <- layout(pa1,
                            xaxis=list(title="Value",range=xl_ai,tickfont=list(size=9)),
                            yaxis=list(title="Density",range=yl_den,tickfont=list(size=9)),
                            legend=list(orientation="h",x=0,y=-0.25,font=list(size=8)),
                            showlegend=TRUE,
                            paper_bgcolor="white", plot_bgcolor="#FAFAFA",
                            margin=list(l=48,r=8,t=40,b=52),
                            title=list(text="<b>Density — All Variables</b>",
                                       font=list(size=11,color="#2C3E50"),x=0.5))

                        # ── A2: Normal Q-Q (all vars) ────────────────────────
                        pa2 <- plot_ly()
                        for (vi in seq_along(results)) {
                            ri <- results[[vi]]
                            qq_r <- qqnorm(ri$data,plot.it=FALSE)
                            q25r <- quantile(ri$data,.25,na.rm=TRUE)
                            q75r <- quantile(ri$data,.75,na.rm=TRUE)
                            sl_q <- (q75r-q25r)/(qnorm(.75)-qnorm(.25))
                            in_q <- q25r-sl_q*qnorm(.25)
                            pa2 <- add_trace(pa2,x=qq_r$x,y=qq_r$y,type="scatter",
                                mode="markers",name=ri$varlab,showlegend=FALSE,
                                marker=list(color=pcol(vi),size=4,opacity=0.7))
                            pa2 <- add_trace(pa2,x=range(qq_r$x),
                                y=in_q+sl_q*range(qq_r$x),type="scatter",mode="lines",
                                showlegend=FALSE,hoverinfo="none",
                                line=list(color=pcol(vi),width=1.5,dash="dot"))
                        }
                        pa2 <- do.call(layout, c(list(pa2,
                            xaxis=list(title="Theoretical Q",tickfont=list(size=9)),
                            yaxis=list(title="Sample Q",tickfont=list(size=9))),
                            ai_lyt("Normal Q-Q — All Variables")))

                        # ── A3: Process vs Spec (first var) ─────────────────
                        xs3 <- seq(xbar1-5*s1,xbar1+5*s1,l=300)
                        if(!is.null(lsl)) xs3[1]           <- min(xs3[1],lsl-s1)
                        if(!is.null(usl)) xs3[length(xs3)] <- max(xs3[length(xs3)],usl+s1)
                        xl3 <- range(c(xs3,lsl,usl),na.rm=TRUE)
                        xl3 <- xl3+diff(xl3)*0.08*c(-1,1)
                        yl3 <- c(0, dnorm(xbar1,xbar1,s1)*1.30)
                        pa3 <- plot_ly(x=xs3,y=dnorm(xs3,xbar1,s1),
                            type="scatter",mode="lines",showlegend=FALSE,
                            fill="tozeroy",fillcolor="#D6EAF888",
                            line=list(color="#2980B9",width=2))
                        if (!is.null(lsl)) pa3 <- add_trace(pa3,x=c(lsl,lsl),y=yl3,
                            type="scatter",mode="lines",showlegend=FALSE,hoverinfo="skip",
                            line=list(color="#E74C3C",width=1.8,dash="dash"))
                        if (!is.null(usl)) pa3 <- add_trace(pa3,x=c(usl,usl),y=yl3,
                            type="scatter",mode="lines",showlegend=FALSE,hoverinfo="skip",
                            line=list(color="#E74C3C",width=1.8,dash="dash"))
                        if (!is.null(target)) pa3 <- add_trace(pa3,x=c(target,target),y=yl3,
                            type="scatter",mode="lines",showlegend=FALSE,hoverinfo="skip",
                            line=list(color="#27AE60",width=1.2,dash="dot"))
                        pa3 <- do.call(layout, c(list(pa3,
                            xaxis=list(title="Value",range=xl3,tickfont=list(size=9)),
                            yaxis=list(title="Density",range=yl3,tickfont=list(size=9))),
                            ai_lyt(paste0("Process vs Spec — ",first_varlab))))

                        # ── A4: Run chart (first var) ────────────────────────
                        yl_run <- range(c(data1,lsl,usl,xbar1),na.rm=TRUE)
                        yl_run <- yl_run+diff(yl_run)*0.18*c(-1,1)
                        pa4 <- plot_ly(x=seq_len(n1),y=data1,type="scatter",
                            mode="lines+markers",showlegend=FALSE,
                            marker=list(color="#2980B9",size=3.5),
                            line=list(color="#2980B9",width=1.2))
                        pa4 <- add_trace(pa4,x=c(1L,n1),y=c(xbar1,xbar1),
                            type="scatter",mode="lines",showlegend=FALSE,hoverinfo="skip",
                            line=list(color="#27AE60",width=1.5,dash="dash"))
                        if (!is.null(lsl)) pa4 <- add_trace(pa4,x=c(1L,n1),y=c(lsl,lsl),
                            type="scatter",mode="lines",showlegend=FALSE,hoverinfo="skip",
                            line=list(color="#E74C3C",width=1.5,dash="dot"))
                        if (!is.null(usl)) pa4 <- add_trace(pa4,x=c(1L,n1),y=c(usl,usl),
                            type="scatter",mode="lines",showlegend=FALSE,hoverinfo="skip",
                            line=list(color="#E74C3C",width=1.5,dash="dot"))
                        pa4 <- do.call(layout, c(list(pa4,
                            xaxis=list(title="Observation",range=c(0.5,n1+0.5),tickfont=list(size=9)),
                            yaxis=list(title="Value",range=yl_run,tickfont=list(size=9))),
                            ai_lyt(paste0("Run Chart — ",first_varlab))))

                        # ── A5: Box plot (all vars) ──────────────────────────
                        yl_box <- range(c(unlist(lapply(results,`[[`,"data")),lsl,usl),na.rm=TRUE)
                        yl_box <- yl_box+diff(yl_box)*0.12*c(-1,1)
                        nv_ai  <- length(results)
                        pa5 <- plot_ly()
                        for (vi in seq_along(results)) {
                            ri <- results[[vi]]
                            pa5 <- add_trace(pa5,y=ri$data,type="box",name=ri$varlab,
                                showlegend=FALSE,boxmean=TRUE,
                                marker=list(color=pcol(vi),size=3),
                                fillcolor=paste0(substr(pcol(vi),1,7),"55"),
                                line=list(color=pcol(vi),width=1))
                        }
                        sh_pa5 <- Filter(Negate(is.null), list(
                            if(!is.null(lsl)) list(type="line",x0=0,x1=1,xref="paper",
                                y0=lsl,y1=lsl,yref="y",
                                line=list(color="#E74C3C",width=1.5,dash="dot")),
                            if(!is.null(usl)) list(type="line",x0=0,x1=1,xref="paper",
                                y0=usl,y1=usl,yref="y",
                                line=list(color="#E74C3C",width=1.5,dash="dot"))))
                        pa5 <- do.call(layout, c(list(pa5,
                            xaxis=list(title="Variable",tickfont=list(size=9),type="category"),
                            yaxis=list(title="Value",range=yl_box,tickfont=list(size=9)),
                            shapes=if(length(sh_pa5)>0) sh_pa5 else NULL),
                            ai_lyt("Box Plot — All Variables")))

                        # ── A6: Summary card (HTML, no plotly needed) ────────
                        sum_rows <- paste(sapply(results, function(ri) {
                            sprintf('<tr><td>%s</td><td>%d</td><td>%.4f</td><td>%.5f</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>',
                                ri$varlab, ri$n, ri$xbar, ri$s,
                                fv_ai(ri$cp), fv_ai(ri$cpk), fv_ai(ri$pp), fv_ai(ri$ppk))
                        }), collapse="")
                        pa6_html <- paste0(
                            '<div style="padding:8px;font-size:12px;color:#2C3E50;height:290px;overflow:auto">',
                            '<b style="font-size:13px">Process Summary</b><br><br>',
                            '<table style="width:100%;border-collapse:collapse;font-size:11px">',
                            '<thead><tr style="background:#2C3E50;color:white">',
                            '<th style="padding:4px 6px;text-align:left">Var</th>',
                            '<th style="padding:4px 6px">n</th><th style="padding:4px 6px">Mean</th>',
                            '<th style="padding:4px 6px">StdDev</th><th style="padding:4px 6px">Cp</th>',
                            '<th style="padding:4px 6px">Cpk</th><th style="padding:4px 6px">Pp</th>',
                            '<th style="padding:4px 6px">Ppk</th></tr></thead>',
                            '<tbody>', sum_rows, '</tbody></table>',
                            if (!is.null(lsl)||!is.null(usl)) paste0(
                                '<br><small style="color:#666">',
                                paste(Filter(nchar,c(
                                    if(!is.null(lsl)) sprintf("LSL=%.4f",lsl),
                                    if(!is.null(usl)) sprintf("USL=%.4f",usl),
                                    if(!is.null(target)) sprintf("Target=%.4f",target))),collapse=" | "),
                                '</small>') else "",
                            '</div>')

                        # ── Render overview as static PNG — guarantees correct layout ──
                        # (plotly table cells bleed in SPSS viewer regardless of resize tricks)
                        ai_png_html <- tryCatch({
                            if (!requireNamespace("base64enc", quietly=TRUE)) stop("no base64enc")
                            tmpf_ai <- tempfile(fileext=".png")
                            png(tmpf_ai, width=1400, height=800, res=110, bg="white")
                            tryCatch({
                                nv <- length(results)
                                col_fn <- function(vi) VAR_PALETTE[((vi-1)%%length(VAR_PALETTE))+1]
                                graphics::layout(matrix(c(1,2,3,4,5,6), nrow=2, byrow=TRUE))
                                par(oma=c(0,0,3,0), mar=c(4,4,3,1), cex=0.85)

                                # P1: Density overlay
                                xseq_ai <- seq(xl_ai[1], xl_ai[2], l=300)
                                yl_dn   <- c(0, max(sapply(results, function(ri)
                                    max(dnorm(xseq_ai, ri$xbar, ri$s))))*1.35)
                                plot(xseq_ai, dnorm(xseq_ai, results[[1]]$xbar, results[[1]]$s),
                                     type="n", xlim=xl_ai, ylim=yl_dn,
                                     xlab="Value", ylab="Density", main="Density — All Variables",
                                     cex.main=0.95, cex.lab=0.85, cex.axis=0.78)
                                for (vi in seq_along(results)) {
                                    ri <- results[[vi]]
                                    yk <- dnorm(xseq_ai, ri$xbar, ri$s)
                                    polygon(c(xseq_ai, rev(xseq_ai)), c(yk, rep(0,length(xseq_ai))),
                                            col=paste0(substr(col_fn(vi),1,7),"22"), border=NA)
                                    lines(xseq_ai, yk, col=col_fn(vi), lwd=2)
                                }
                                if(!is.null(lsl))    abline(v=lsl,    col="#E74C3C", lwd=1.8, lty=2)
                                if(!is.null(usl))    abline(v=usl,    col="#E74C3C", lwd=1.8, lty=2)
                                if(!is.null(target)) abline(v=target, col="#27AE60", lwd=1.5, lty=3)
                                legend("topleft", sapply(results, `[[`, "varlab"),
                                       col=sapply(seq_along(results), col_fn),
                                       lwd=2, cex=0.7, bty="n")
                                grid(col="grey92", lty=1)

                                # P2: Normal Q-Q all vars
                                all_th <- unlist(lapply(results, function(ri) qqnorm(ri$data,plot.it=FALSE)$x))
                                all_sa <- unlist(lapply(results, function(ri) qqnorm(ri$data,plot.it=FALSE)$y))
                                plot(all_th, all_sa, type="n",
                                     xlab="Theoretical Q", ylab="Sample Q", main="Normal Q-Q — All Variables",
                                     cex.main=0.95, cex.lab=0.85, cex.axis=0.78)
                                for (vi in seq_along(results)) {
                                    ri  <- results[[vi]]
                                    qq  <- qqnorm(ri$data, plot.it=FALSE)
                                    q25 <- quantile(ri$data,.25); q75 <- quantile(ri$data,.75)
                                    sl  <- (q75-q25)/(qnorm(.75)-qnorm(.25))
                                    points(qq$x, qq$y, col=col_fn(vi), pch=20, cex=0.5)
                                    abline(a=q25-sl*qnorm(.25), b=sl, col=col_fn(vi), lty=2, lwd=1.5)
                                }
                                abline(v=0, col="grey30", lwd=1)
                                legend("topleft", sapply(results, `[[`, "varlab"),
                                       col=sapply(seq_along(results), col_fn),
                                       pch=20, cex=0.7, bty="n")
                                grid(col="grey92", lty=1)

                                # P3: Process vs Spec (first var)
                                r1   <- results[[1]]
                                xseq3 <- seq(xl_ai[1], xl_ai[2], l=300)
                                yl3   <- c(0, max(dnorm(xseq3, r1$xbar, r1$s))*1.35)
                                plot(xseq3, dnorm(xseq3, r1$xbar, r1$s), type="l",
                                     col="#2980B9", lwd=2.5, xlim=xl_ai, ylim=yl3,
                                     xlab="Value", ylab="Density",
                                     main=paste0("Process vs Spec — ", r1$varlab),
                                     cex.main=0.95, cex.lab=0.85, cex.axis=0.78)
                                polygon(c(xseq3, rev(xseq3)),
                                        c(dnorm(xseq3,r1$xbar,r1$s), rep(0,length(xseq3))),
                                        col="#2980B922", border=NA)
                                if(!is.null(lsl))    abline(v=lsl,    col="#E74C3C", lwd=2, lty=2)
                                if(!is.null(usl))    abline(v=usl,    col="#E74C3C", lwd=2, lty=2)
                                if(!is.null(target)) abline(v=target, col="#27AE60", lwd=1.5, lty=3)
                                grid(col="grey92", lty=1)

                                # P4: Run chart (first var)
                                n1r <- r1$n; dat1 <- r1$data
                                yl_r <- range(c(dat1,lsl,usl),na.rm=TRUE)
                                yl_r <- yl_r + diff(yl_r)*0.12*c(-1,1)
                                plot(seq_len(n1r), dat1, type="l", col="#2980B9", lwd=1.2,
                                     ylim=yl_r, xlab="Observation", ylab="Value",
                                     main=paste0("Run Chart — ", r1$varlab),
                                     cex.main=0.95, cex.lab=0.85, cex.axis=0.78)
                                points(seq_len(n1r), dat1, pch=20, col="#2980B9", cex=0.5)
                                abline(h=r1$xbar, col="#27AE60", lwd=1.8)
                                if(!is.null(lsl))    abline(h=lsl, col="#E74C3C", lwd=1.5, lty=2)
                                if(!is.null(usl))    abline(h=usl, col="#E74C3C", lwd=1.5, lty=2)
                                grid(col="grey92", lty=1)

                                # P5: Box plot all vars
                                yl_bx <- range(c(unlist(lapply(results,`[[`,"data")),lsl,usl),na.rm=TRUE)
                                yl_bx <- yl_bx + diff(yl_bx)*0.12*c(-1,1)
                                boxplot(lapply(results, `[[`, "data"),
                                        names=sapply(results,`[[`,"varlab"),
                                        col=sapply(seq_along(results), function(vi) paste0(substr(col_fn(vi),1,7),"88")),
                                        border=sapply(seq_along(results), col_fn),
                                        ylim=yl_bx, xlab="Variable", ylab="Value",
                                        main="Box Plot — All Variables",
                                        cex.main=0.95, cex.lab=0.85, cex.axis=0.78,
                                        pch=20, outcex=0.7)
                                if(!is.null(lsl))    abline(h=lsl, col="#E74C3C", lwd=1.5, lty=2)
                                if(!is.null(usl))    abline(h=usl, col="#E74C3C", lwd=1.5, lty=2)
                                if(!is.null(target)) abline(h=target, col="#27AE60", lwd=1.5, lty=3)
                                grid(col="grey92", lty=1)

                                # P6: Summary table as text
                                par(mar=c(1,1,3,1))
                                plot.new()
                                rect(0,0,1,1, col="#F8F9FA", border="#D5D8DC")
                                title(main="Process Summary", cex.main=0.95, col.main="#2C3E50", font.main=2)
                                fv6 <- function(v) if(!is.null(v)&&length(v)==1&&!is.na(v)) formatC(v,4,format="f") else "N/A"
                                hdr6  <- sprintf("%-10s %5s %7s %7s %7s %7s", "Var","n","Cp","Cpk","Pp","Ppk")
                                rows6 <- sapply(results, function(ri)
                                    sprintf("%-10s %5d %7s %7s %7s %7s",
                                        substr(ri$varlab,1,10), ri$n,
                                        fv6(ri$cp), fv6(ri$cpk), fv6(ri$pp), fv6(ri$ppk)))
                                all_txt <- c(hdr6, rep("",1), rows6)
                                if (!is.null(lsl)||!is.null(usl)) all_txt <- c(all_txt, "",
                                    paste(Filter(nchar, c(
                                        if(!is.null(lsl)) sprintf("LSL=%.4f",lsl),
                                        if(!is.null(usl)) sprintf("USL=%.4f",usl),
                                        if(!is.null(target)) sprintf("Target=%.4f",target)
                                    )), collapse=" | "))
                                text(0.05, seq(0.90, max(0.05, 0.90-(length(all_txt)-1)*0.085),
                                              l=length(all_txt)),
                                     all_txt, adj=c(0,0.5), cex=0.72, family="mono",
                                     col=c("#2C3E50",rep("#444",length(all_txt)-1)),
                                     font=c(2,rep(1,length(all_txt)-1)))

                                mtext("Capability Overview — All Variables",
                                      outer=TRUE, cex=1.1, font=2, col="#2C3E50", line=1.2)

                                if (!is.null(logo_img)) tryCatch(
                                    grid::grid.raster(logo_img, x=0.985, y=0.985,
                                        width=grid::unit(0.07,"npc"), height=grid::unit(0.055,"npc"),
                                        just=c("right","top")),
                                    error=function(e) invisible(NULL))
                            }, error=function(e) NULL)
                            dev.off()
                            b64_ai <- base64enc::base64encode(tmpf_ai)
                            tryCatch(file.remove(tmpf_ai), error=function(e) NULL)
                            paste0(
                                '<h3 style="color:#2C3E50;margin:0 0 10px 0;font-size:15px;',
                                'border-left:4px solid #3498DB;padding-left:10px">',
                                'Capability Overview — All Variables</h3>',
                                sprintf('<img src="data:image/png;base64,%s" style="width:100%%;border-radius:4px">',
                                        b64_ai))
                        }, error=function(e_ai) {
                            sprintf('<div style="color:#c0392b;padding:8px"><b>Overview PNG error:</b> %s</div>',
                                    e_ai$message)
                        })
                        plt_divs <- c(plt_divs, ai_png_html)
                    }, error=function(e_ai) {
                        plt_divs <<- c(plt_divs, sprintf(
                            '<div style="color:#c0392b;padding:12px;border:1px solid #e74c3c;border-radius:4px;margin:8px 0"><b>Capability Overview chart error:</b> %s</div>',
                            gsub("&","&amp;",gsub("<","&lt;",e_ai$message))))
                        warns$warn(paste("Overview chart error:",e_ai$message))
                    })

                    # ── Chart 1: Capability Histogram (all vars, x-axis fixed) ──
                    {
                        xl_h2 <- range(c(unlist(lapply(results,`[[`,"data")),lsl,usl),na.rm=TRUE)
                        xl_h2 <- xl_h2 + diff(xl_h2)*0.18*c(-1,1)
                        bin_h2 <- diff(xl_h2)/max(5L,min(40L,ceiling(sqrt(n1))))
                        p_hist <- plot_ly()
                        for (vi in seq_along(results)) {
                            ri <- results[[vi]]
                            xk2 <- seq(xl_h2[1], xl_h2[2], l=300)
                            # Pre-compute density histogram (avoids add_histogram histnorm/xbins warnings)
                            brks_h2 <- seq(xl_h2[1], xl_h2[2], by=bin_h2)
                            hh2 <- hist(ri$data, breaks=brks_h2, plot=FALSE)
                            p_hist <- add_bars(p_hist,
                                x=hh2$mids, y=hh2$density, name=ri$varlab,
                                opacity=0.55, showlegend=TRUE, width=bin_h2*0.95,
                                marker=list(color=pcol(vi),line=list(color="white",width=0.4)))
                            p_hist <- add_trace(p_hist, x=xk2, y=dnorm(xk2,ri$xbar,ri$s),
                                type="scatter", mode="lines", name=paste0(ri$varlab," fit"),
                                showlegend=FALSE, line=list(color=pcol(vi),width=2.5))
                        }
                        sh_h2 <- Filter(Negate(is.null), list(
                            if(!is.null(lsl)) list(type="line",x0=lsl,x1=lsl,y0=0,y1=1,
                                yref="paper",xref="x",name="spec_limit",line=list(color="#E74C3C",width=2,dash="dash")),
                            if(!is.null(usl)) list(type="line",x0=usl,x1=usl,y0=0,y1=1,
                                yref="paper",xref="x",name="spec_limit",line=list(color="#E74C3C",width=2,dash="dash")),
                            if(!is.null(target)) list(type="line",x0=target,x1=target,y0=0,y1=1,
                                yref="paper",xref="x",line=list(color="#27AE60",width=1.5,dash="dot"))
                        ))
                        # Per-variable statistical control limits (x̄ ± 2.66·MR̄), drawn
                        # as thin colour-matched vertical dashed lines so each variable's
                        # band is distinguishable. Tagged "ctrl_limit" so the UCL/LCL
                        # toggle button can hide them (4 vars = 8 lines can get busy).
                        sh_h2_ctrl <- list()
                        for (vi in seq_along(results)) {
                            ri <- results[[vi]]
                            if (is.null(ri$imr_data)) next
                            sh_h2_ctrl <- c(sh_h2_ctrl, list(
                                list(type="line",x0=ri$imr_data$i_ucl,x1=ri$imr_data$i_ucl,
                                    y0=0,y1=1,yref="paper",xref="x",name="ctrl_limit",
                                    line=list(color=pcol(vi),width=1,dash="dot")),
                                list(type="line",x0=ri$imr_data$i_lcl,x1=ri$imr_data$i_lcl,
                                    y0=0,y1=1,yref="paper",xref="x",name="ctrl_limit",
                                    line=list(color=pcol(vi),width=1,dash="dot"))
                            ))
                        }
                        sh_h2 <- c(sh_h2, sh_h2_ctrl)
                        ann_h2 <- Filter(Negate(is.null), list(
                            if(!is.null(lsl)) list(x=lsl,y=1,xref="x",yref="paper",text="LSL",
                                showarrow=FALSE,xanchor="right",yanchor="top",name="spec_limit",font=list(color="#E74C3C",size=11)),
                            if(!is.null(usl)) list(x=usl,y=1,xref="x",yref="paper",text="USL",
                                showarrow=FALSE,xanchor="left",yanchor="top",name="spec_limit",font=list(color="#E74C3C",size=11))
                        ))
                        # Legend-only dummy traces explaining the reference lines
                        # (shapes never show in the legend on their own).
                        if (!is.null(lsl) || !is.null(usl) || !is.null(target))
                            p_hist <- add_trace(p_hist, x=c(NA), y=c(NA), type="scatter",
                                mode="lines", name="LSL/USL/Target (spec limits)", showlegend=TRUE,
                                hoverinfo="skip", line=list(color="#E74C3C",width=2,dash="dash"))
                        if (length(sh_h2_ctrl) > 0)
                            p_hist <- add_trace(p_hist, x=c(NA), y=c(NA), type="scatter",
                                mode="lines", name="UCL/LCL per variable (control limits)",
                                showlegend=TRUE, hoverinfo="skip",
                                line=list(color="#7F8C8D",width=1,dash="dot"))
                        p_hist <- layout(p_hist,
                            title=list(text=paste0("Capability Histogram — ", all_vars_lbl),font=list(size=15)),
                            xaxis=list(title="Measurement", range=xl_h2),
                            yaxis=list(title="Density"),
                            barmode="overlay", shapes=sh_h2, annotations=ann_h2,
                            legend=list(orientation="h",y=-0.18),
                            paper_bgcolor="white", plot_bgcolor="#FAFAFA",
                            margin=list(l=60,r=40,t=60,b=90))
                        plt_divs <- c(plt_divs, plt_div(p_hist,"plt_hist",450,"Capability Histogram",
                                                         has_ctrl_limits=(length(sh_h2_ctrl) > 0), has_spec_limits=has_specs))
                    }

                    # ── Chart 2: Normal Q-Q ───────────────────────────────────
                    {
                        p_qq <- plot_ly()
                        for (vi in seq_along(results)) {
                            ri   <- results[[vi]]
                            qq_r <- qqnorm(ri$data, plot.it=FALSE)
                            q25q <- quantile(ri$data,.25,na.rm=TRUE)
                            q75q <- quantile(ri$data,.75,na.rm=TRUE)
                            sl_q <- (q75q-q25q)/(qnorm(.75)-qnorm(.25))
                            in_q <- q25q - sl_q*qnorm(.25)
                            p_qq <- add_trace(p_qq, x=qq_r$x, y=qq_r$y,
                                type="scatter", mode="markers", name=ri$varlab,
                                marker=list(color=pcol(vi),size=5,opacity=0.75))
                            p_qq <- add_trace(p_qq, x=range(qq_r$x),
                                y=in_q+sl_q*range(qq_r$x),
                                type="scatter", mode="lines",
                                name=paste0(ri$varlab," line"), showlegend=FALSE,
                                line=list(color=pcol(vi),width=2,dash="dot"))
                        }
                        p_qq <- layout(p_qq,
                            title=list(text=paste0("Normal Q-Q Plot — ", all_vars_lbl),font=list(size=15)),
                            xaxis=list(title="Theoretical Quantiles"),
                            yaxis=list(title="Sample Quantiles"),
                            legend=list(orientation="h",y=-0.18),
                            paper_bgcolor="white", plot_bgcolor="#FAFAFA",
                            margin=list(l=60,r=40,t=60,b=90))
                        plt_divs <- c(plt_divs, plt_div(p_qq,"plt_qq",430,paste0("Normal Q-Q Plot — ", all_vars_lbl)))
                    }

                    # ── Chart 3: Run charts ───────────────────────────────────
                    {
                        n_v_r <- length(results)
                        sp_run <- lapply(seq_along(results), function(vi) {
                            ri  <- results[[vi]]
                            obs <- seq_len(ri$n)
                            p_r <- plot_ly(x=obs, y=ri$data, type="scatter",
                                mode="lines+markers", name=ri$varlab, showlegend=TRUE,
                                marker=list(color=pcol(vi),size=5),
                                line=list(color=pcol(vi),width=1.5))
                            # Statistical control limits (x̄ ± 2.66·MR̄, the standard
                            # Individuals-chart formula — same calculation used for the
                            # I-MR charts) — distinct from the spec limits (LSL/USL,
                            # which come from the user-supplied specification, not the
                            # data). Tagged "ctrl_limit" so the toggle button can
                            # show/hide just these.
                            ctrl_r <- ri$imr_data
                            sh_r <- Filter(Negate(is.null), list(
                                list(type="line",x0=1,x1=ri$n,y0=ri$xbar,y1=ri$xbar,
                                    xref="x",yref="y",line=list(color="#27AE60",width=1.5,dash="dash")),
                                if(!is.null(ctrl_r)) list(type="line",x0=1,x1=ri$n,
                                    y0=ctrl_r$i_ucl,y1=ctrl_r$i_ucl,xref="x",yref="y",
                                    name="ctrl_limit",
                                    line=list(color="#E74C3C",width=1.2,dash="dash")),
                                if(!is.null(ctrl_r)) list(type="line",x0=1,x1=ri$n,
                                    y0=ctrl_r$i_lcl,y1=ctrl_r$i_lcl,xref="x",yref="y",
                                    name="ctrl_limit",
                                    line=list(color="#E74C3C",width=1.2,dash="dash")),
                                if(!is.null(lsl)) list(type="line",x0=1,x1=ri$n,y0=lsl,y1=lsl,
                                    xref="x",yref="y",name="spec_limit",line=list(color="#9B59B6",width=1.5,dash="dot")),
                                if(!is.null(usl)) list(type="line",x0=1,x1=ri$n,y0=usl,y1=usl,
                                    xref="x",yref="y",name="spec_limit",line=list(color="#9B59B6",width=1.5,dash="dot"))
                            ))
                            # Label each reference line with its name AND value
                            # (was just unlabeled lines — confusing which was
                            # which). yshift keeps text clear of the line itself.
                            ann_r <- Filter(Negate(is.null), list(
                                list(x=ri$n,y=ri$xbar,xref="x",yref="y",showarrow=FALSE,
                                    text=sprintf("CL=%.4f",ri$xbar), xanchor="right",
                                    yshift=10, font=list(size=9,color="#27AE60")),
                                if(!is.null(ctrl_r)) list(x=ri$n,y=ctrl_r$i_ucl,xref="x",yref="y",
                                    showarrow=FALSE, text=sprintf("UCL=%.4f",ctrl_r$i_ucl),
                                    xanchor="right", yshift=10, name="ctrl_limit",
                                    font=list(size=9,color="#E74C3C")),
                                if(!is.null(ctrl_r)) list(x=ri$n,y=ctrl_r$i_lcl,xref="x",yref="y",
                                    showarrow=FALSE, text=sprintf("LCL=%.4f",ctrl_r$i_lcl),
                                    xanchor="right", yshift=-10, name="ctrl_limit",
                                    font=list(size=9,color="#E74C3C")),
                                if(!is.null(lsl)) list(x=1,y=lsl,xref="x",yref="y",showarrow=FALSE,
                                    text=sprintf("LSL=%.4f",lsl), xanchor="left",
                                    yshift=-10, name="spec_limit", font=list(size=9,color="#9B59B6")),
                                if(!is.null(usl)) list(x=1,y=usl,xref="x",yref="y",showarrow=FALSE,
                                    text=sprintf("USL=%.4f",usl), xanchor="left",
                                    yshift=10, name="spec_limit", font=list(size=9,color="#9B59B6"))
                            ))
                            layout(p_r, yaxis=list(title=ri$varlab,titlefont=list(size=11)),
                                   shapes=sh_r, annotations=ann_r)
                        })
                        p_run_all <- subplot(sp_run, nrows=n_v_r, shareX=TRUE, titleY=TRUE,
                                             heights=rep(1/n_v_r, n_v_r))
                        # Dummy legend-only traces explaining what each reference line
                        # means (the lines themselves are `shapes` and would not
                        # otherwise appear in the legend).
                        p_run_all <- add_trace(p_run_all, x=c(NA), y=c(NA), type="scatter",
                            mode="lines", name="Center Line (x̄)", showlegend=TRUE,
                            hoverinfo="skip", line=list(color="#27AE60",width=1.5,dash="dash"))
                        p_run_all <- add_trace(p_run_all, x=c(NA), y=c(NA), type="scatter",
                            mode="lines", name="UCL/LCL (control limits)", showlegend=TRUE,
                            hoverinfo="skip", line=list(color="#E74C3C",width=1.2,dash="dash"))
                        if (!is.null(lsl) || !is.null(usl))
                            p_run_all <- add_trace(p_run_all, x=c(NA), y=c(NA), type="scatter",
                                mode="lines", name="LSL/USL (spec limits)", showlegend=TRUE,
                                hoverinfo="skip", line=list(color="#9B59B6",width=1.5,dash="dot"))
                        p_run_all <- layout(p_run_all,
                            title=list(text=paste0("Run Charts — ", all_vars_lbl),font=list(size=15)),
                            xaxis=list(title="Observation"),
                            paper_bgcolor="white", plot_bgcolor="#FAFAFA",
                            showlegend=TRUE,
                            legend=list(orientation="h", x=0.5, xanchor="center",
                                        y=-(0.55/n_v_r), font=list(size=10)),
                            margin=list(l=70,r=50,t=85,b=70))
                        plt_divs <- c(plt_divs,
                            plt_div(p_run_all,"plt_run",max(350,n_v_r*200),
                                    "Run Charts — All Variables", is_subplot=TRUE,
                                    has_ctrl_limits=TRUE, has_spec_limits=has_specs))
                    }

                    # ── Chart 3b: Phase-segmented run charts ─────────────────
                    # Companion to the pooled run charts above: for each
                    # variable with a usable phase split, redraw its run
                    # sequence with separate center-line / control-limit
                    # SEGMENTS per phase — vertical dashed separators at the
                    # phase boundaries, labels centered over each phase's own
                    # span, and the final/most-recent phase highlighted —
                    # mirroring the treatment used for the phase-segmented
                    # I-MR chart. Limits use the identical formula
                    # (x̄ ± 2.66·MR̄), recomputed on each phase's own data
                    # subset (see phase_stats).
                    for (vi8 in seq_along(results)) {
                      tryCatch({
                        rv8 <- results[[vi8]]
                        if (is.null(rv8$phase_stats) || rv8$n < 2) next
                        ph8  <- rv8$phase_stats
                        obs8 <- seq_len(rv8$n)
                        # See show_lbl7 note (Chart 5b): suppress inline
                        # labels when there are too many short phases to
                        # render legibly — segments/limit lines stay visible,
                        # and exact values remain in the By-Phase table.
                        show_lbl8 <- length(ph8) <= 12

                        # Real-coordinate y-range (data + every phase's CL/UCL/
                        # LCL + spec limits) for the phase-boundary separator
                        # lines below — use yref="y" (not "paper") so these
                        # mix safely with the other yref="y" shapes on this
                        # plot (mixing "y" and "paper" refs in one shapes list
                        # has been observed to make plotly choke with "invalid
                        # 'replacement' argument" when the figure is built).
                        r8_yrng <- range(c(rv8$data, lsl, usl,
                                           unlist(lapply(ph8, function(p) c(p$xbar,p$i_ucl,p$i_lcl)))),
                                         na.rm=TRUE)
                        r8_yrng <- r8_yrng + diff(r8_yrng) * 0.06 * c(-1, 1)

                        p_r8 <- plot_ly(x=obs8, y=rv8$data, type="scatter",
                            mode="lines+markers", name=rv8$varlab, showlegend=FALSE,
                            marker=list(color="#2980B9",size=5),
                            line=list(color="#2980B9",width=1.5))

                        sh_r8  <- list()
                        ann_r8 <- list()
                        for (pi8 in seq_along(ph8)) {
                            ph <- ph8[[pi8]]
                            x0 <- min(ph$idx); x1 <- max(ph$idx)
                            xm <- mean(c(x0, x1))
                            lim_color <- if (ph$is_final) "#C0392B" else "#E74C3C"
                            lim_width <- if (ph$is_final) 2.0 else 1.2
                            lbl_size  <- if (ph$is_final) 10 else 9
                            sh_r8 <- c(sh_r8, list(
                                list(type="line",x0=x0,x1=x1,y0=ph$xbar,y1=ph$xbar,
                                    xref="x",yref="y",
                                    line=list(color="#27AE60",width=1.4,dash="dash")),
                                list(type="line",x0=x0,x1=x1,y0=ph$i_ucl,y1=ph$i_ucl,
                                    xref="x",yref="y",name="ctrl_limit",
                                    line=list(color=lim_color,width=lim_width,dash="dash")),
                                list(type="line",x0=x0,x1=x1,y0=ph$i_lcl,y1=ph$i_lcl,
                                    xref="x",yref="y",name="ctrl_limit",
                                    line=list(color=lim_color,width=lim_width,dash="dash"))
                            ))
                            if (show_lbl8) ann_r8 <- c(ann_r8, list(
                                list(x=xm,y=ph$xbar,xref="x",yref="y",showarrow=FALSE,
                                    text=sprintf("%s: CL=%.4f",ph$label,ph$xbar),
                                    yshift=10, font=list(size=9,color="#27AE60")),
                                list(x=xm,y=ph$i_ucl,xref="x",yref="y",showarrow=FALSE,
                                    text=sprintf("%s: UCL=%.4f",ph$label,ph$i_ucl),
                                    yshift=10, name="ctrl_limit",
                                    font=list(size=lbl_size,color=lim_color)),
                                list(x=xm,y=ph$i_lcl,xref="x",yref="y",showarrow=FALSE,
                                    text=sprintf("%s: LCL=%.4f",ph$label,ph$i_lcl),
                                    yshift=-10, name="ctrl_limit",
                                    font=list(size=lbl_size,color=lim_color))
                            ))
                            if (pi8 > 1)
                                sh_r8 <- c(sh_r8, list(list(type="line",
                                    x0=x0-0.5, x1=x0-0.5, y0=r8_yrng[1], y1=r8_yrng[2],
                                    xref="x", yref="y",
                                    line=list(color="#7F8C8D",width=1.3,dash="dot"))))
                        }
                        if (!is.null(lsl))
                            sh_r8 <- c(sh_r8, list(list(type="line",x0=1,x1=rv8$n,y0=lsl,y1=lsl,
                                xref="x",yref="y",name="spec_limit",line=list(color="#9B59B6",width=1.5,dash="dot"))))
                        if (!is.null(usl))
                            sh_r8 <- c(sh_r8, list(list(type="line",x0=1,x1=rv8$n,y0=usl,y1=usl,
                                xref="x",yref="y",name="spec_limit",line=list(color="#9B59B6",width=1.5,dash="dot"))))

                        p_r8 <- layout(p_r8,
                            title=list(text=paste0("Run Chart — By Phase (",
                                                    as.character(groupvar),") — ",rv8$varlab),
                                       font=list(size=15)),
                            yaxis=list(title=rv8$varlab, range=r8_yrng), xaxis=list(title="Observation"),
                            shapes=sh_r8, annotations=ann_r8,
                            paper_bgcolor="white", plot_bgcolor="#FAFAFA",
                            margin=list(l=70,r=90,t=70,b=50))
                        plt_divs <- c(plt_divs,
                            plt_div(p_r8,paste0("plt_run_phase_",vi8),420,
                                    paste0("Run Chart — By Phase — ",rv8$varlab),
                                    has_ctrl_limits=TRUE, has_spec_limits=has_specs))
                      }, error = function(e) {
                          .cl <- tryCatch(paste(deparse(conditionCall(e)), collapse=" "),
                                          error=function(e2) "<no call>")
                          warns$warn(sprintf(
                              "[DIAG] Phase Run Chart failed for variable #%d (%s): %s | call: %s",
                              vi8,
                              tryCatch(results[[vi8]]$varlab, error=function(e3) "?"),
                              conditionMessage(e), .cl))
                      })
                    }

                    # ── Chart 4a: Violin (all variables) ─────────────────────
                    {
                        yl_vio <- range(c(unlist(lapply(results,`[[`,"data")),lsl,usl),na.rm=TRUE)
                        yl_vio <- yl_vio + diff(yl_vio)*0.15*c(-1,1)
                        p_vio <- plot_ly()
                        for (vi in seq_along(results)) {
                            ri <- results[[vi]]
                            p_vio <- add_trace(p_vio, y=ri$data, type="violin",
                                name=ri$varlab, box=list(visible=TRUE),
                                meanline=list(visible=TRUE,color="white",width=2),
                                fillcolor=paste0(substr(pcol(vi),1,7),"88"),
                                line=list(color=pcol(vi),width=1.5),
                                points="outliers", marker=list(color=pcol(vi),size=4))
                        }
                        sh_vio2 <- Filter(Negate(is.null), list(
                            if(!is.null(lsl)) list(type="line",x0=0,x1=1,xref="paper",
                                y0=lsl,y1=lsl,yref="y",name="spec_limit",
                                line=list(color="#E74C3C",width=2,dash="dash")),
                            if(!is.null(usl)) list(type="line",x0=0,x1=1,xref="paper",
                                y0=usl,y1=usl,yref="y",name="spec_limit",
                                line=list(color="#E74C3C",width=2,dash="dash")),
                            if(!is.null(target)) list(type="line",x0=0,x1=1,xref="paper",
                                y0=target,y1=target,yref="y",
                                line=list(color="#27AE60",width=1.5,dash="dot"))
                        ))
                        # Reference lines (LSL/USL/Target) are drawn as `shapes`, which
                        # never show in the legend. Add invisible-data dummy traces with
                        # matching name/line style so the legend explains what each
                        # dashed/dotted line means.
                        if (!is.null(lsl)) p_vio <- add_trace(p_vio, x=c(NA), y=c(NA),
                            type="scatter", mode="lines", name="LSL [spec_limit]", showlegend=TRUE,
                            hoverinfo="skip", line=list(color="#E74C3C",width=2,dash="dash"))
                        if (!is.null(usl)) p_vio <- add_trace(p_vio, x=c(NA), y=c(NA),
                            type="scatter", mode="lines", name="USL [spec_limit]", showlegend=TRUE,
                            hoverinfo="skip", line=list(color="#E74C3C",width=2,dash="dash"))
                        if (!is.null(target)) p_vio <- add_trace(p_vio, x=c(NA), y=c(NA),
                            type="scatter", mode="lines", name="Target", showlegend=TRUE,
                            hoverinfo="skip", line=list(color="#27AE60",width=1.5,dash="dot"))
                        p_vio <- layout(p_vio,
                            title=list(text=paste0("Violin Distribution — ", all_vars_lbl),font=list(size=15)),
                            yaxis=list(title="Value", range=yl_vio),
                            xaxis=list(title="Variable"),
                            shapes=sh_vio2,
                            legend=list(orientation="h",y=-0.18),
                            paper_bgcolor="white", plot_bgcolor="#FAFAFA",
                            margin=list(l=60,r=40,t=60,b=90))
                        plt_divs <- c(plt_divs,
                            plt_div(p_vio,"plt_violin",460,paste0("Violin Distribution — ", all_vars_lbl), has_spec_limits=has_specs))
                    }

                    # ── Chart 4b: Box plot (all variables, separate) ──────────
                    {
                        yl_box2 <- range(c(unlist(lapply(results,`[[`,"data")),lsl,usl),na.rm=TRUE)
                        yl_box2 <- yl_box2 + diff(yl_box2)*0.12*c(-1,1)
                        p_box2 <- plot_ly()
                        for (vi in seq_along(results)) {
                            ri <- results[[vi]]
                            p_box2 <- add_trace(p_box2, y=ri$data, type="box",
                                name=ri$varlab, showlegend=TRUE, boxmean=TRUE,
                                marker=list(color=pcol(vi),size=4,outliercolor="#E74C3C"),
                                fillcolor=paste0(substr(pcol(vi),1,7),"55"),
                                line=list(color=pcol(vi),width=1.2))
                        }
                        sh_box2 <- Filter(Negate(is.null), list(
                            if(!is.null(lsl)) list(type="line",x0=0,x1=1,xref="paper",
                                y0=lsl,y1=lsl,yref="y",name="spec_limit",
                                line=list(color="#E74C3C",width=2,dash="dash")),
                            if(!is.null(usl)) list(type="line",x0=0,x1=1,xref="paper",
                                y0=usl,y1=usl,yref="y",name="spec_limit",
                                line=list(color="#E74C3C",width=2,dash="dash")),
                            if(!is.null(target)) list(type="line",x0=0,x1=1,xref="paper",
                                y0=target,y1=target,yref="y",
                                line=list(color="#27AE60",width=1.5,dash="dot"))
                        ))
                        # Same as Violin: spec-limit/target lines are `shapes` and won't
                        # appear in the legend on their own — add labeled dummy traces.
                        if (!is.null(lsl)) p_box2 <- add_trace(p_box2, x=c(NA), y=c(NA),
                            type="scatter", mode="lines", name="LSL [spec_limit]", showlegend=TRUE,
                            hoverinfo="skip", line=list(color="#E74C3C",width=2,dash="dash"))
                        if (!is.null(usl)) p_box2 <- add_trace(p_box2, x=c(NA), y=c(NA),
                            type="scatter", mode="lines", name="USL [spec_limit]", showlegend=TRUE,
                            hoverinfo="skip", line=list(color="#E74C3C",width=2,dash="dash"))
                        if (!is.null(target)) p_box2 <- add_trace(p_box2, x=c(NA), y=c(NA),
                            type="scatter", mode="lines", name="Target", showlegend=TRUE,
                            hoverinfo="skip", line=list(color="#27AE60",width=1.5,dash="dot"))
                        p_box2 <- layout(p_box2,
                            title=list(text=paste0("Box Plot — ", all_vars_lbl),font=list(size=15)),
                            yaxis=list(title="Value", range=yl_box2),
                            xaxis=list(title="Variable"),
                            shapes=sh_box2,
                            legend=list(orientation="h",y=-0.2,font=list(size=10)),
                            paper_bgcolor="white", plot_bgcolor="#FAFAFA",
                            margin=list(l=60,r=40,t=60,b=95))
                        plt_divs <- c(plt_divs,
                            plt_div(p_box2,"plt_box",420,"Box Plot — All Variables", has_spec_limits=has_specs))
                    }

                    # ── Chart 5: I-MR per variable ────────────────────────────
                    for (vi6 in seq_along(results)) {
                        rv6 <- results[[vi6]]
                        if (is.null(rv6$imr_data) || rv6$n < 2) next
                        imrd6 <- rv6$imr_data; mr6 <- imrd6$mr
                        i_ooc6  <- which(rv6$data > imrd6$i_ucl | rv6$data < imrd6$i_lcl)
                        mr_ooc6 <- which(mr6 > imrd6$mr_ucl)
                        obs6 <- seq_len(rv6$n)

                        p_i6 <- plot_ly(x=obs6, y=rv6$data, type="scatter",
                            mode="lines+markers", name="Individuals",
                            marker=list(color="#2980B9",size=6),
                            line=list(color="#2980B9",width=1.5))
                        if (length(i_ooc6) > 0)
                            p_i6 <- add_trace(p_i6, x=i_ooc6, y=rv6$data[i_ooc6],
                                type="scatter", mode="markers", name="OOC",
                                marker=list(color="#E74C3C",size=13,symbol="circle-open",
                                            line=list(width=2.5,color="#E74C3C")))
                        sh_i6 <- Filter(Negate(is.null), list(
                            list(type="line",x0=1,x1=rv6$n,y0=rv6$xbar,y1=rv6$xbar,
                                xref="x",yref="y",line=list(color="#27AE60",width=2)),
                            list(type="line",x0=1,x1=rv6$n,y0=imrd6$i_ucl,y1=imrd6$i_ucl,
                                xref="x",yref="y",name="ctrl_limit",
                                line=list(color="#E74C3C",width=1.5,dash="dash")),
                            list(type="line",x0=1,x1=rv6$n,y0=imrd6$i_lcl,y1=imrd6$i_lcl,
                                xref="x",yref="y",name="ctrl_limit",
                                line=list(color="#E74C3C",width=1.5,dash="dash")),
                            if(!is.null(lsl)) list(type="line",x0=1,x1=rv6$n,y0=lsl,y1=lsl,
                                xref="x",yref="y",name="spec_limit",line=list(color="#9B59B6",width=1.5,dash="dot")),
                            if(!is.null(usl)) list(type="line",x0=1,x1=rv6$n,y0=usl,y1=usl,
                                xref="x",yref="y",name="spec_limit",line=list(color="#9B59B6",width=1.5,dash="dot"))
                        ))
                        # NOTE: yshift offsets each label vertically off its own
                        # reference line so the dashed/solid line doesn't run straight
                        # through the text (was rendering as garbled strikethrough text).
                        ann_i6 <- list(
                            list(x=rv6$n,y=rv6$xbar,text=sprintf("CL=%.4f",rv6$xbar),
                                xref="x",yref="y",showarrow=FALSE,xanchor="right",
                                yshift=11, font=list(size=10,color="#27AE60")),
                            list(x=rv6$n,y=imrd6$i_ucl,text=sprintf("UCL=%.4f",imrd6$i_ucl),
                                xref="x",yref="y",showarrow=FALSE,xanchor="right",
                                yshift=11, name="ctrl_limit", font=list(size=10,color="#E74C3C")),
                            list(x=rv6$n,y=imrd6$i_lcl,text=sprintf("LCL=%.4f",imrd6$i_lcl),
                                xref="x",yref="y",showarrow=FALSE,xanchor="right",
                                yshift=-11, name="ctrl_limit", font=list(size=10,color="#E74C3C"))
                        )
                        p_i6 <- layout(p_i6, yaxis=list(title="Individual Value"),
                            xaxis=list(title="Observation"), shapes=sh_i6, annotations=ann_i6,
                            paper_bgcolor="white", plot_bgcolor="#FAFAFA",
                            legend=list(orientation="h",y=-0.25),
                            margin=list(l=65,r=90,t=10,b=40))

                        p_mr6 <- plot_ly(x=seq_len(rv6$n-1), y=mr6, type="scatter",
                            mode="lines+markers", name="Moving Range",
                            marker=list(color="#E67E22",size=6),
                            line=list(color="#E67E22",width=1.5), showlegend=TRUE)
                        if (length(mr_ooc6) > 0)
                            p_mr6 <- add_trace(p_mr6, x=mr_ooc6, y=mr6[mr_ooc6],
                                type="scatter", mode="markers", name="MR OOC", showlegend=TRUE,
                                marker=list(color="#E74C3C",size=13,symbol="circle-open",
                                            line=list(width=2.5,color="#E74C3C")))
                        # Moving-Range LCL is mathematically 0 (D3=0 for subgroup size
                        # 2), which sits exactly on the x-axis and was effectively
                        # invisible — the user assumed it was "missing". Drawing it
                        # explicitly + labeling it makes clear it IS there, just at zero.
                        sh_mr6 <- list(
                            list(type="line",x0=1,x1=rv6$n-1,y0=imrd6$mr_bar,y1=imrd6$mr_bar,
                                xref="x",yref="y",line=list(color="#27AE60",width=2)),
                            list(type="line",x0=1,x1=rv6$n-1,y0=imrd6$mr_ucl,y1=imrd6$mr_ucl,
                                xref="x",yref="y",name="ctrl_limit",
                                line=list(color="#E74C3C",width=1.5,dash="dash")),
                            list(type="line",x0=1,x1=rv6$n-1,y0=0,y1=0,
                                xref="x",yref="y",name="ctrl_limit",
                                line=list(color="#E74C3C",width=1.5,dash="dot"))
                        )
                        ann_mr6 <- list(
                            list(x=rv6$n-1,y=imrd6$mr_bar,text=sprintf("CL (MR̄)=%.4f",imrd6$mr_bar),
                                xref="x",yref="y",showarrow=FALSE,xanchor="right",
                                yshift=11, font=list(size=10,color="#27AE60")),
                            list(x=rv6$n-1,y=imrd6$mr_ucl,text=sprintf("UCL=%.4f",imrd6$mr_ucl),
                                xref="x",yref="y",showarrow=FALSE,xanchor="right",
                                yshift=11, name="ctrl_limit", font=list(size=10,color="#E74C3C")),
                            list(x=rv6$n-1,y=0,text="LCL=0.0000",
                                xref="x",yref="y",showarrow=FALSE,xanchor="right",
                                yshift=11, name="ctrl_limit", font=list(size=10,color="#E74C3C"))
                        )
                        p_mr6 <- layout(p_mr6,
                            yaxis=list(title="Moving Range",rangemode="tozero"),
                            xaxis=list(title="Observation"),
                            shapes=sh_mr6, annotations=ann_mr6,
                            paper_bgcolor="white", plot_bgcolor="#FAFAFA",
                            margin=list(l=65,r=90,t=10,b=40))

                        # Taller, more separated subplots (was c(0.60,0.40) at 520px —
                        # values were cramped/illegible). Extra figure height + bigger
                        # gap between the two panels makes both charts readable.
                        p_imr6 <- subplot(p_i6, p_mr6, nrows=2, shareX=FALSE,
                                          titleY=TRUE, heights=c(0.55,0.45), margin=0.08)
                        p_imr6 <- layout(p_imr6,
                            title=list(text=paste0("I-MR Control Chart — ",rv6$varlab),font=list(size=15)),
                            paper_bgcolor="white",
                            showlegend=TRUE,
                            legend=list(orientation="h", x=0.5, xanchor="center",
                                        y=-0.14, font=list(size=10)),
                            margin=list(l=65,r=90,t=85,b=70))
                        plt_divs <- c(plt_divs,
                            plt_div(p_imr6,paste0("plt_imr_",vi6),680,
                                    paste0("I-MR Control Chart — ",rv6$varlab), is_subplot=TRUE,
                                    has_ctrl_limits=TRUE, has_spec_limits=has_specs))
                    }

                    # ── Chart 5b: Phase-segmented I-MR per variable ───────────
                    # When a grouping variable splits the sequence into more
                    # than one phase, draw a SECOND I-MR chart with separate
                    # UCL/CL/LCL reference-line SEGMENTS for each phase
                    # (vertical dashed separators mark the phase boundaries;
                    # the final/most-recent phase's limits are highlighted in
                    # a stronger color and larger label so the current process
                    # state stands out). This is purely a visual decomposition
                    # of the SAME validated formulas used for the overall chart
                    # above (x̄ ± 2.66·MR̄ for I, 3.267·MR̄ for MR UCL) — each
                    # phase's limits are recomputed on its own data subset (see
                    # phase_stats, computed alongside imr_data, lines ~1185+).
                    for (vi7 in seq_along(results)) {
                      tryCatch({
                        rv7 <- results[[vi7]]
                        if (is.null(rv7$phase_stats) || rv7$n < 2) next
                        ph7  <- rv7$phase_stats
                        obs7 <- seq_len(rv7$n)
                        i_ooc7 <- which(rv7$data > rv7$imr_data$i_ucl | rv7$data < rv7$imr_data$i_lcl)
                        # With many short phases (e.g., a rotating subgroup ID
                        # producing dozens of runs), per-segment text labels
                        # would overlap into an unreadable smear — so beyond a
                        # threshold we keep drawing every segment/limit line
                        # (nothing is hidden numerically; the table in section
                        # "Process Capability — By Phase" still lists every
                        # phase's exact values) but suppress the inline labels.
                        show_lbl7 <- length(ph7) <= 12

                        # Full y-range of each panel (data + every phase's own
                        # CL/UCL/LCL) — used so the phase-boundary separator
                        # lines can be drawn with yref="y"/"y" (real data
                        # coordinates) rather than yref="paper". Plotly's
                        # subplot() does not reliably remap "paper"-referenced
                        # shapes when combining figures (it can throw "invalid
                        # 'replacement' argument" while bumping axis refs), so
                        # within subplot-composed figures every shape must use
                        # a real x/y axis reference instead.
                        i_yrng7 <- range(c(rv7$data,
                                           unlist(lapply(ph7, function(p) c(p$xbar,p$i_ucl,p$i_lcl)))),
                                         na.rm=TRUE)
                        i_yrng7 <- i_yrng7 + diff(i_yrng7) * 0.06 * c(-1, 1)
                        mr7_all <- rv7$imr_data$mr
                        mr_yrng7 <- range(c(mr7_all, 0,
                                            unlist(lapply(ph7, function(p) c(p$mr_bar,p$mr_ucl)))),
                                          na.rm=TRUE)
                        mr_yrng7 <- mr_yrng7 + diff(mr_yrng7) * 0.06 * c(-1, 1)
                        mr_yrng7[1] <- max(mr_yrng7[1], 0)

                        p_i7 <- plot_ly(x=obs7, y=rv7$data, type="scatter",
                            mode="lines+markers", name="Individuals",
                            marker=list(color="#2980B9",size=6),
                            line=list(color="#2980B9",width=1.5))
                        if (length(i_ooc7) > 0)
                            p_i7 <- add_trace(p_i7, x=i_ooc7, y=rv7$data[i_ooc7],
                                type="scatter", mode="markers", name="OOC",
                                marker=list(color="#E74C3C",size=13,symbol="circle-open",
                                            line=list(width=2.5,color="#E74C3C")))

                        sh_i7  <- list()
                        ann_i7 <- list()
                        for (pi7 in seq_along(ph7)) {
                            ph <- ph7[[pi7]]
                            x0 <- min(ph$idx); x1 <- max(ph$idx)
                            xm <- mean(c(x0, x1))
                            # Final phase highlighted with a stronger color,
                            # thicker line, and larger label text — it
                            # represents the current/most-recent process state.
                            lim_color <- if (ph$is_final) "#C0392B" else "#E74C3C"
                            lim_width <- if (ph$is_final) 2.2 else 1.3
                            lbl_size  <- if (ph$is_final) 11 else 9
                            sh_i7 <- c(sh_i7, list(
                                list(type="line",x0=x0,x1=x1,y0=ph$xbar,y1=ph$xbar,
                                    xref="x",yref="y",line=list(color="#27AE60",width=1.6)),
                                list(type="line",x0=x0,x1=x1,y0=ph$i_ucl,y1=ph$i_ucl,
                                    xref="x",yref="y",name="ctrl_limit",
                                    line=list(color=lim_color,width=lim_width,dash="dash")),
                                list(type="line",x0=x0,x1=x1,y0=ph$i_lcl,y1=ph$i_lcl,
                                    xref="x",yref="y",name="ctrl_limit",
                                    line=list(color=lim_color,width=lim_width,dash="dash"))
                            ))
                            # UCL/CL/LCL printed just above (UCL/CL) or below
                            # (LCL) each phase's own segment, centered over
                            # that segment's span — not the chart edge — so
                            # each phase's values are easy to associate with
                            # its stretch of the sequence. (Suppressed when
                            # there are too many short phases to label legibly
                            # — see show_lbl7 above; the segments/limit lines
                            # themselves are still drawn either way.)
                            if (show_lbl7) ann_i7 <- c(ann_i7, list(
                                list(x=xm,y=ph$xbar,xref="x",yref="y",showarrow=FALSE,
                                    text=sprintf("%s: CL=%.4f",ph$label,ph$xbar),
                                    yshift=11, font=list(size=9,color="#27AE60")),
                                list(x=xm,y=ph$i_ucl,xref="x",yref="y",showarrow=FALSE,
                                    text=sprintf("%s: UCL=%.4f",ph$label,ph$i_ucl),
                                    yshift=11, name="ctrl_limit",
                                    font=list(size=lbl_size,color=lim_color)),
                                list(x=xm,y=ph$i_lcl,xref="x",yref="y",showarrow=FALSE,
                                    text=sprintf("%s: LCL=%.4f",ph$label,ph$i_lcl),
                                    yshift=-11, name="ctrl_limit",
                                    font=list(size=lbl_size,color=lim_color))
                            ))
                            # Vertical dashed separator at each phase boundary
                            # (skipped for the first phase, whose "boundary"
                            # is just the chart's left edge).
                            if (pi7 > 1)
                                sh_i7 <- c(sh_i7, list(list(type="line",
                                    x0=x0-0.5, x1=x0-0.5, y0=i_yrng7[1], y1=i_yrng7[2],
                                    xref="x", yref="y",
                                    line=list(color="#7F8C8D",width=1.3,dash="dot"))))
                        }
                        p_i7 <- layout(p_i7, yaxis=list(title="Individual Value", range=i_yrng7),
                            xaxis=list(title="Observation"), shapes=sh_i7, annotations=ann_i7,
                            paper_bgcolor="white", plot_bgcolor="#FAFAFA",
                            legend=list(orientation="h",y=-0.25),
                            margin=list(l=65,r=90,t=10,b=40))

                        sh_mr7  <- list()
                        ann_mr7 <- list()
                        for (pi7 in seq_along(ph7)) {
                            ph <- ph7[[pi7]]
                            if (ph$n < 2) next
                            # MR points fall BETWEEN consecutive observations,
                            # so a phase of m points contributes m-1 MR values;
                            # its segment spans the corresponding MR-axis range.
                            mx0 <- min(ph$idx); mx1 <- max(ph$idx) - 1
                            if (mx1 < mx0) next
                            xm <- mean(c(mx0, mx1))
                            lim_color <- if (ph$is_final) "#C0392B" else "#E74C3C"
                            lim_width <- if (ph$is_final) 2.2 else 1.3
                            lbl_size  <- if (ph$is_final) 11 else 9
                            sh_mr7 <- c(sh_mr7, list(
                                list(type="line",x0=mx0,x1=mx1,y0=ph$mr_bar,y1=ph$mr_bar,
                                    xref="x",yref="y",line=list(color="#27AE60",width=1.6)),
                                list(type="line",x0=mx0,x1=mx1,y0=ph$mr_ucl,y1=ph$mr_ucl,
                                    xref="x",yref="y",name="ctrl_limit",
                                    line=list(color=lim_color,width=lim_width,dash="dash"))
                            ))
                            if (show_lbl7) ann_mr7 <- c(ann_mr7, list(
                                list(x=xm,y=ph$mr_bar,xref="x",yref="y",showarrow=FALSE,
                                    text=sprintf("%s: CL=%.4f",ph$label,ph$mr_bar),
                                    yshift=11, font=list(size=9,color="#27AE60")),
                                list(x=xm,y=ph$mr_ucl,xref="x",yref="y",showarrow=FALSE,
                                    text=sprintf("%s: UCL=%.4f",ph$label,ph$mr_ucl),
                                    yshift=11, name="ctrl_limit",
                                    font=list(size=lbl_size,color=lim_color))
                            ))
                            if (pi7 > 1)
                                sh_mr7 <- c(sh_mr7, list(list(type="line",
                                    x0=mx0-0.5, x1=mx0-0.5, y0=mr_yrng7[1], y1=mr_yrng7[2],
                                    xref="x", yref="y",
                                    line=list(color="#7F8C8D",width=1.3,dash="dot"))))
                        }
                        mr7 <- rv7$imr_data$mr
                        p_mr7 <- plot_ly(x=seq_len(rv7$n-1), y=mr7, type="scatter",
                            mode="lines+markers", name="Moving Range",
                            marker=list(color="#E67E22",size=6),
                            line=list(color="#E67E22",width=1.5), showlegend=TRUE)
                        p_mr7 <- layout(p_mr7,
                            yaxis=list(title="Moving Range",rangemode="tozero",range=mr_yrng7),
                            xaxis=list(title="Observation"),
                            shapes=sh_mr7, annotations=ann_mr7,
                            paper_bgcolor="white", plot_bgcolor="#FAFAFA",
                            margin=list(l=65,r=90,t=10,b=40))

                        p_imr7 <- subplot(p_i7, p_mr7, nrows=2, shareX=FALSE,
                                          titleY=TRUE, heights=c(0.55,0.45), margin=0.08)
                        p_imr7 <- layout(p_imr7,
                            title=list(text=paste0("I-MR Control Chart — By Phase (",
                                                    as.character(groupvar),") — ",rv7$varlab),
                                       font=list(size=15)),
                            paper_bgcolor="white",
                            showlegend=TRUE,
                            legend=list(orientation="h", x=0.5, xanchor="center",
                                        y=-0.14, font=list(size=10)),
                            margin=list(l=65,r=90,t=85,b=70))
                        plt_divs <- c(plt_divs,
                            plt_div(p_imr7,paste0("plt_imr_phase_",vi7),680,
                                    paste0("I-MR Control Chart — By Phase — ",rv7$varlab),
                                    is_subplot=TRUE, has_ctrl_limits=TRUE, has_spec_limits=has_specs))
                      }, error = function(e) {
                          .cl <- tryCatch(paste(deparse(conditionCall(e)), collapse=" "),
                                          error=function(e2) "<no call>")
                          warns$warn(sprintf(
                              "[DIAG] Phase I-MR Chart failed for variable #%d (%s): %s | call: %s",
                              vi7,
                              tryCatch(results[[vi7]]$varlab, error=function(e3) "?"),
                              conditionMessage(e), .cl))
                      })
                    }

                    # ── Chart 6: Capability distribution per variable ─────────
                    for (vi_h in seq_along(results)) {
                        local({
                            ri_h  <- results[[vi_h]]
                            dat_h <- ri_h$data; n_h <- ri_h$n
                            if (n_h < 3) return(invisible(NULL))
                            xb_h <- ri_h$xbar; sw_h <- ri_h$s_within; so_h <- ri_h$s
                            cp_h <- ri_h$cp;  cpk_h <- ri_h$cpk
                            pp_h <- ri_h$pp;  ppk_h <- ri_h$ppk
                            cpl_h <- ri_h$cpl; cpu_h <- ri_h$cpu
                            ppl_h <- ri_h$cpl_o; ppu_h <- ri_h$cpu_o; cpm_h <- ri_h$cpm

                            xl_h <- range(c(dat_h,lsl,usl),na.rm=TRUE)
                            xl_h <- xl_h + diff(xl_h)*0.18*c(-1,1)
                            xseq_h <- seq(xl_h[1],xl_h[2],length.out=500)
                            bin_h  <- diff(xl_h)/max(8L,min(30L,ceiling(sqrt(n_h)*1.5)))

                            p_cap_h <- plot_ly()
                            # Pre-compute density histogram (avoids add_histogram histnorm/xbins warnings)
                            brks_cap <- seq(xl_h[1], xl_h[2], by=bin_h)
                            hh_cap   <- hist(dat_h, breaks=brks_cap, plot=FALSE)
                            p_cap_h <- add_bars(p_cap_h,
                                x=hh_cap$mids, y=hh_cap$density, name="Data", opacity=0.65,
                                width=bin_h*0.95,
                                marker=list(color="#AED6F1",line=list(color="white",width=0.5)))
                            p_cap_h <- add_trace(p_cap_h, x=xseq_h,
                                y=dnorm(xseq_h,xb_h,sw_h), type="scatter", mode="lines",
                                name="Within (Potential)", line=list(color="#2C3E50",width=2.5))
                            p_cap_h <- add_trace(p_cap_h, x=xseq_h,
                                y=dnorm(xseq_h,xb_h,so_h), type="scatter", mode="lines",
                                name="Overall", line=list(color="#E74C3C",width=2.5,dash="dash"))

                            sh_cap2 <- Filter(Negate(is.null), list(
                                if(!is.null(lsl)) list(type="line",x0=lsl,x1=lsl,y0=0,y1=1,
                                    yref="paper",xref="x",name="spec_limit",line=list(color="#E74C3C",width=2.5)),
                                if(!is.null(usl)) list(type="line",x0=usl,x1=usl,y0=0,y1=1,
                                    yref="paper",xref="x",name="spec_limit",line=list(color="#E74C3C",width=2.5)),
                                if(!is.null(target)) list(type="line",x0=target,x1=target,
                                    y0=0,y1=1,yref="paper",xref="x",
                                    line=list(color="#27AE60",width=1.5,dash="dot"))
                            ))
                            ann_cap2 <- Filter(Negate(is.null), list(
                                if(!is.null(lsl)) list(x=lsl,y=1,xref="x",yref="paper",
                                    text="LSL",showarrow=FALSE,xanchor="right",name="spec_limit",
                                    font=list(color="#E74C3C",size=11)),
                                if(!is.null(usl)) list(x=usl,y=1,xref="x",yref="paper",
                                    text="USL",showarrow=FALSE,xanchor="left",name="spec_limit",
                                    font=list(color="#E74C3C",size=11))
                            ))
                            fh <- function(v,d=4)
                                if(!is.null(v)&&length(v)==1&&!is.na(v))
                                    formatC(v,d,format="f") else "—"

                            grade_v <- if(!is.null(cpk_h)&&!is.na(cpk_h)) cpk_h
                                       else if(!is.null(ppk_h)&&!is.na(ppk_h)) ppk_h else NA
                            grade_h <- if(is.na(grade_v)) "Ungraded"
                                       else if(grade_v>=1.67) "Excellent"
                                       else if(grade_v>=1.33) "Capable"
                                       else if(grade_v>=1.00) "Marginal"
                                       else "Not Capable"
                            grade_col <- switch(grade_h,"Excellent"="#27AE60","Capable"="#2980B9",
                                                "Marginal"="#E67E22","#E74C3C")

                            sw_p_h <- tryCatch(
                                shapiro.test(if(n_h>5000) sample(dat_h,5000) else dat_h)$p.value,
                                error=function(e) NA)

                            # ── Clean plotly histogram (no annotation blob) ──────
                            p_cap_h <- layout(p_cap_h,
                                title=list(
                                    text=paste0("<b>Capability Report — ",ri_h$varlab,"</b>"),
                                    font=list(size=13,color="#2C3E50")),
                                xaxis=list(title="Value",range=xl_h,tickfont=list(size=10)),
                                yaxis=list(title="Density",tickfont=list(size=10)),
                                shapes=sh_cap2,
                                annotations=ann_cap2,
                                legend=list(orientation="h",x=0.5,xanchor="center",y=-0.22,
                                            font=list(size=10)),
                                paper_bgcolor="white", plot_bgcolor="#FAFAFA",
                                margin=list(l=55,r=20,t=45,b=70))

                            # ── PPM data for bottom table ─────────────────────
                            ppm_below  <- if(!is.null(ri_h$ppm_below)&&!is.na(ri_h$ppm_below))
                                              sprintf("%.1f",ri_h$ppm_below) else "—"
                            ppm_above  <- if(!is.null(ri_h$ppm_above)&&!is.na(ri_h$ppm_above))
                                              sprintf("%.1f",ri_h$ppm_above) else "—"
                            ppm_tot    <- if(!is.null(ri_h$ppm_total)&&!is.na(ri_h$ppm_total))
                                              sprintf("%.1f",ri_h$ppm_total) else "—"
                            exp_b_ov   <- if(!is.null(lsl)&&!is.na(so_h))
                                              sprintf("%.1f",pnorm(lsl,xb_h,so_h)*1e6) else "—"
                            exp_a_ov   <- if(!is.null(usl)&&!is.na(so_h))
                                              sprintf("%.1f",(1-pnorm(usl,xb_h,so_h))*1e6) else "—"
                            exp_t_ov   <- if(!is.null(lsl)||!is.null(usl)) {
                                              b <- if(!is.null(lsl)&&!is.na(so_h)) pnorm(lsl,xb_h,so_h) else 0
                                              a <- if(!is.null(usl)&&!is.na(so_h)) 1-pnorm(usl,xb_h,so_h) else 0
                                              sprintf("%.1f",(b+a)*1e6)
                                          } else "—"
                            exp_b_wi   <- if(!is.null(lsl)&&!is.na(sw_h))
                                              sprintf("%.1f",pnorm(lsl,xb_h,sw_h)*1e6) else "—"
                            exp_a_wi   <- if(!is.null(usl)&&!is.na(sw_h))
                                              sprintf("%.1f",(1-pnorm(usl,xb_h,sw_h))*1e6) else "—"
                            exp_t_wi   <- if(!is.null(lsl)||!is.null(usl)) {
                                              b <- if(!is.null(lsl)&&!is.na(sw_h)) pnorm(lsl,xb_h,sw_h) else 0
                                              a <- if(!is.null(usl)&&!is.na(sw_h)) 1-pnorm(usl,xb_h,sw_h) else 0
                                              sprintf("%.1f",(b+a)*1e6)
                                          } else "—"

                            sw_dec  <- if(!is.na(sw_p_h)) if(sw_p_h>0.05) "Normal" else "Non-normal" else "—"
                            sw_dcol <- if(!is.na(sw_p_h)&&sw_p_h<=0.05) "#E74C3C" else "#27AE60"

                            # ── Normality annotation on Plotly histogram (captured by camera button) ──
                            if (!is.na(sw_p_h)) {
                                p_cap_h <- add_annotations(p_cap_h,
                                    x = 0.01, y = 0.99, xref = "paper", yref = "paper",
                                    text = sprintf("S-W: p=%.4f  %s", sw_p_h, sw_dec),
                                    showarrow = FALSE, xanchor = "left", yanchor = "top",
                                    font = list(size = 10, color = sw_dcol),
                                    bgcolor = "rgba(255,255,255,0.80)",
                                    bordercolor = sw_dcol, borderwidth = 1, borderpad = 3)
                            }

                            # ── HTML helper: gauge bar ────────────────────────
                            gauge_bar <- function(label, val, max_val=2.0) {
                                v <- if(!is.null(val)&&length(val)==1&&!is.na(val)) val else NA
                                if (is.na(v)) {
                                    return(sprintf(
                                        '<tr><td style="font-size:10px;color:#555;padding:2px 3px;width:28px">%s</td>
<td style="padding:2px 3px"><div style="background:#ECF0F1;height:11px;border-radius:2px"></div></td>
<td style="font-size:10px;color:#888;padding:2px 3px;text-align:right;width:44px">—</td></tr>',
                                        label))
                                }
                                pct     <- min(100, max(0, v/max_val*100))
                                col     <- if(v>=1.33) "#27AE60" else if(v>=1.0) "#E67E22" else "#E74C3C"
                                ref_pct <- min(100, 1.33/max_val*100)
                                sprintf(
                                    '<tr><td style="font-size:10px;color:#555;padding:2px 3px;width:28px">%s</td>
<td style="padding:2px 3px"><div style="position:relative;background:#ECF0F1;height:11px;border-radius:2px">
  <div style="width:%.1f%%;height:100%%;background:%s;border-radius:2px"></div>
  <div style="position:absolute;top:-1px;left:%.1f%%;width:2px;height:13px;background:#555;opacity:0.35"></div>
</div></td>
<td style="font-size:10px;color:%s;font-weight:bold;padding:2px 3px;text-align:right;width:44px">%s</td></tr>',
                                    label, pct, col, ref_pct, col, formatC(v,4,format="f"))
                            }

                            # ── Left panel: Process Data ──────────────────────
                            pd_row <- function(k,v)
                                sprintf('<tr><td style="color:#666;font-size:11px;padding:2px 4px">%s</td><td style="font-size:11px;font-weight:bold;padding:2px 4px;text-align:right">%s</td></tr>',k,v)
                            left_html <- paste0(
                                '<div style="background:#EBF5FB;border:1px solid #AED6F1;border-radius:4px;padding:8px;margin-bottom:6px">',
                                '<div style="font-size:12px;font-weight:bold;color:#2C3E50;margin-bottom:4px;border-bottom:1px solid #AED6F1;padding-bottom:3px">Process Data</div>',
                                '<table style="width:100%;border-collapse:collapse">',
                                if(!is.null(lsl)) pd_row("LSL", formatC(lsl,4,format="f")) else "",
                                if(!is.null(target)) pd_row("Target", formatC(target,4,format="f")) else "",
                                if(!is.null(usl)) pd_row("USL", formatC(usl,4,format="f")) else "",
                                pd_row("Mean", formatC(xb_h,4,format="f")),
                                pd_row("n", as.character(n_h)),
                                pd_row("SD (Overall)", formatC(so_h,5,format="f")),
                                pd_row("SD (Within)", formatC(sw_h,5,format="f")),
                                pd_row("Sigma", sprintf("%gσ", sigma_mult)),
                                '</table></div>',
                                '<div style="background:#EBF5FB;border:1px solid #AED6F1;border-radius:4px;padding:8px;margin-bottom:6px">',
                                '<div style="font-size:12px;font-weight:bold;color:#2C3E50;margin-bottom:4px;border-bottom:1px solid #AED6F1;padding-bottom:3px">Normality (Shapiro-Wilk)</div>',
                                '<table style="width:100%;border-collapse:collapse">',
                                pd_row("W statistic", if(!is.na(sw_p_h)) {
                                    ww <- tryCatch(shapiro.test(if(n_h>5000) sample(dat_h,5000) else dat_h)$statistic, error=function(e) NA)
                                    if(!is.na(ww)) formatC(ww,4,format="f") else "—"} else "—"),
                                pd_row("p-value", if(!is.na(sw_p_h)) sprintf('<span style="color:%s">%.4f</span>',sw_dcol,sw_p_h) else "—"),
                                pd_row("Decision", sprintf('<span style="color:%s;font-weight:bold">%s</span>',sw_dcol,sw_dec)),
                                '</table></div>',
                                sprintf('<div style="background:%s;border-radius:4px;padding:10px;text-align:center">',grade_col),
                                '<div style="font-size:11px;color:white;opacity:0.85;margin-bottom:4px">Process Grade</div>',
                                sprintf('<div style="font-size:14px;font-weight:bold;color:white">%s</div>',grade_h),
                                '<div style="font-size:9px;color:white;opacity:0.75;margin-top:6px;border-top:1px solid rgba(255,255,255,0.35);padding-top:4px;line-height:1.6">',
                                '≥ 1.67 Excellent &nbsp;•&nbsp; ≥ 1.33 Capable &nbsp;•&nbsp; ≥ 1.00 Marginal &nbsp;•&nbsp; &lt; 1.00 Not Capable',
                                '</div>',
                                '</div>')

                            # ── Right panel: Gauge bars ───────────────────────
                            right_html <- paste0(
                                '<div style="border:1px solid #D5EEF8;border-radius:4px;padding:6px;margin-bottom:6px">',
                                '<div style="font-size:11px;font-weight:bold;color:#2C3E50;margin-bottom:3px;border-bottom:1px solid #D5EEF8;padding-bottom:2px">Overall Cap.</div>',
                                '<table style="width:100%;border-collapse:collapse">',
                                gauge_bar("Pp",   pp_h),
                                gauge_bar("PPL",  ppl_h),
                                gauge_bar("PPU",  ppu_h),
                                gauge_bar("Ppk",  ppk_h),
                                gauge_bar("Cpm",  cpm_h),
                                '</table></div>',
                                '<div style="border:1px solid #D5EEF8;border-radius:4px;padding:6px">',
                                '<div style="font-size:11px;font-weight:bold;color:#2C3E50;margin-bottom:3px;border-bottom:1px solid #D5EEF8;padding-bottom:2px">Within Cap.</div>',
                                '<table style="width:100%;border-collapse:collapse">',
                                gauge_bar("Cp",   cp_h),
                                gauge_bar("CPL",  cpl_h),
                                gauge_bar("CPU",  cpu_h),
                                gauge_bar("Cpk",  cpk_h),
                                '</table>',
                                if(!is.na(cpk_h)) {
                                    ci_pct_h <- if (!is.null(conf) && is.finite(conf) && conf > 0 && conf < 1) conf*100 else 95
                                    z_ci_h   <- qnorm(1 - (1 - ci_pct_h/100)/2)
                                    ci_lo <- cpk_h - z_ci_h * sqrt(1/(9*n_h)+cpk_h^2/(2*(n_h-1)))
                                    ci_hi <- cpk_h + z_ci_h * sqrt(1/(9*n_h)+cpk_h^2/(2*(n_h-1)))
                                    sprintf('<div style="font-size:10px;color:#888;margin-top:4px">Cpk %g%% CI: [%.2f, %.2f]</div>', ci_pct_h, ci_lo, ci_hi)
                                } else "",
                                '</div>')

                            # ── Numeric within-sigma PPM (for Yield and Sigma Level columns)
                            # Mirrors the R graphics footer: z_lt derived from within-based PPM,
                            # not from Ppk*3 which would use s_total (overall sigma).
                            wi_ppm_num <- {
                                b <- if (!is.null(lsl) && is.finite(sw_h) && sw_h > 0) pnorm(lsl, xb_h, sw_h) else 0
                                a <- if (!is.null(usl) && is.finite(sw_h) && sw_h > 0) 1 - pnorm(usl, xb_h, sw_h) else 0
                                (b + a) * 1e6
                            }
                            z_lt_wi_num <- if (is.finite(wi_ppm_num) && wi_ppm_num > 0 && wi_ppm_num < 1e6)
                                               -qnorm(wi_ppm_num / 1e6) else NA_real_
                            z_st_wi_num <- if (!is.na(z_lt_wi_num)) z_lt_wi_num + 1.5 else NA_real_

                            # ── Bottom: PPM performance table ─────────────────
                            th_s <- 'style="background:#34495E;color:white;font-size:11px;padding:5px 8px;text-align:center"'
                            td_s <- 'style="font-size:11px;padding:4px 8px;text-align:center;border-bottom:1px solid #ECF0F1"'
                            td_b <- 'style="font-size:11px;padding:4px 8px;text-align:center;border-bottom:1px solid #ECF0F1;font-weight:bold;background:#2C3E5022"'
                            bot_html <- paste0(
                                '<table style="width:100%;border-collapse:collapse;margin-top:4px">',
                                '<thead><tr>',
                                sprintf('<th %s style="background:#34495E;color:white;font-size:11px;padding:5px 8px;text-align:left">Metric</th>',th_s),
                                sprintf('<th %s>Observed</th>',th_s),
                                sprintf('<th %s>Exp. (Overall)</th>',th_s),
                                sprintf('<th %s>Exp. (Within)</th>',th_s),
                                sprintf('<th %s>DPMO</th>',th_s),
                                sprintf('<th %s>Yield %%</th>',th_s),
                                sprintf('<th %s>Sigma Level (Within)</th>',th_s),
                                '</tr></thead><tbody>',
                                sprintf('<tr><td %s>PPM &lt; LSL</td><td %s>%s</td><td %s>%s</td><td %s>%s</td><td %s>—</td><td %s>—</td><td %s>—</td></tr>',
                                    td_s, td_s,ppm_below, td_s,exp_b_ov, td_s,exp_b_wi, td_s, td_s, td_s),
                                sprintf('<tr><td %s>PPM &gt; USL</td><td %s>%s</td><td %s>%s</td><td %s>%s</td><td %s>—</td><td %s>—</td><td %s>—</td></tr>',
                                    td_s, td_s,ppm_above, td_s,exp_a_ov, td_s,exp_a_wi, td_s, td_s, td_s),
                                sprintf('<tr><td %s>PPM Total</td><td %s>%s</td><td %s>%s</td><td %s>%s</td><td %s>%s</td><td %s>%s</td><td %s>%s</td></tr>',
                                    td_b, td_b,ppm_tot, td_b,exp_t_ov, td_b,exp_t_wi,
                                    td_b, if(!is.null(ri_h$ppm_total)&&!is.na(ri_h$ppm_total)) sprintf("%.1f",ri_h$ppm_total) else "—",
                                    td_b, if (is.finite(wi_ppm_num) && wi_ppm_num >= 0 && wi_ppm_num < 1e6) sprintf("%.4f%%", (1-wi_ppm_num/1e6)*100) else "—",
                                    td_b, if (!is.na(z_st_wi_num)) sprintf("Z_st = %.2f  Z_lt = %.2f", z_st_wi_num, z_lt_wi_num) else "—"),
                                '</tbody></table>')

                            # ── Assemble full standard SPC-style card ──────────────
                            card_id  <- paste0("caprep_",vi_h)
                            safe_lab <- gsub("[^A-Za-z0-9_]","_",ri_h$varlab)
                            hist_div <- plt_div(p_cap_h, paste0("plt_cap_",vi_h), 380, "", has_spec_limits=(!is.null(lsl)||!is.null(usl)))

                            # Pre-render full static capability report as PNG (no CDN needed)
                            dl_btn <- ""
                            tryCatch({
                                if (requireNamespace("base64enc", quietly=TRUE)) {
                                    tmpf_cap <- tempfile(fileext=".png")
                                    png(tmpf_cap, width=1400, height=850, res=120, bg="white")
                                    tryCatch({
                                        # Row heights: panels top 70%, PPM table bottom 30%
                                        layout_mat <- matrix(c(1,1,2,3,
                                                               1,1,4,5,
                                                               6,6,6,6),
                                                             nrow=3, byrow=TRUE)
                                        graphics::layout(layout_mat, widths=c(2,2,1.5,1.5),
                                               heights=c(3,3,1.5))
                                        par(oma=c(0,0,4,0), cex=0.85)

                                        # Panel 1: Histogram + density
                                        par(mar=c(4,4,2,1))
                                        xseq_s <- seq(xl_h[1],xl_h[2],l=400)
                                        hist(dat_h, freq=FALSE, breaks="Sturges",
                                             col="#AED6F1", border="white",
                                             xlim=xl_h, ylim=c(0,max(dnorm(xseq_s,xb_h,min(sw_h,so_h)))*1.3),
                                             xlab="Value", main="Capability Distribution",
                                             cex.lab=0.9, cex.axis=0.8)
                                        lines(xseq_s, dnorm(xseq_s,xb_h,sw_h), col="#2C3E50", lwd=2.5)
                                        lines(xseq_s, dnorm(xseq_s,xb_h,so_h), col="#E74C3C", lwd=2.5, lty=2)
                                        if(!is.null(lsl))    abline(v=lsl,    col="#E74C3C", lwd=2, lty=2)
                                        if(!is.null(usl))    abline(v=usl,    col="#E74C3C", lwd=2, lty=2)
                                        if(!is.null(target)) abline(v=target, col="#27AE60", lwd=1.5, lty=3)
                                        legend("topright", c("Within","Overall"),
                                               col=c("#2C3E50","#E74C3C"), lwd=2, lty=c(1,2),
                                               cex=0.75, bty="n")
                                        grid(col="grey92", lty=1)

                                        # Panel 2: Process Data text
                                        par(mar=c(1,1,2,1))
                                        plot.new()
                                        rect(0,0,1,1, col="#EBF5FB", border="#AED6F1")
                                        pd_items <- c(
                                            "Process Data",
                                            if(!is.null(lsl))    sprintf("LSL:    %.4f", lsl)    else NULL,
                                            if(!is.null(target)) sprintf("Target: %.4f", target)  else NULL,
                                            if(!is.null(usl))    sprintf("USL:    %.4f", usl)     else NULL,
                                            sprintf("Mean:   %.4f", xb_h),
                                            sprintf("n:      %d",   n_h),
                                            sprintf("SD(ov): %.5f", so_h),
                                            sprintf("SD(wi): %.5f", sw_h),
                                            sprintf("Sigma:  %g", sigma_mult),
                                            "",
                                            "Normality (S-W)",
                                            sprintf("p = %.4f  %s", if(!is.na(sw_p_h)) sw_p_h else 0, sw_dec)
                                        )
                                        text(0.05, seq(0.95, 0.05, l=length(pd_items)),
                                             pd_items, adj=c(0,1), cex=0.75,
                                             col=c("#2C3E50",rep("#444",length(pd_items)-1)),
                                             font=c(2,rep(1,length(pd_items)-1)))

                                        # Panel 3: Grade box
                                        par(mar=c(1,1,2,1))
                                        plot.new()
                                        rect(0,0,1,1, col=grade_col, border=NA)
                                        text(0.5, 0.72, "Process Grade", col="white", cex=0.85, font=2)
                                        text(0.5, 0.50, grade_h, col="white", cex=1.1, font=2)
                                        abline(h=0.30, col=adjustcolor("white",0.35), lwd=0.8)
                                        text(0.5, 0.20,
                                             paste0("≥1.67 Excellent  ≥1.33 Capable\n",
                                                    "≥1.00 Marginal  <1.00 Not Capable"),
                                             col=adjustcolor("white",0.80), cex=0.55, font=1)

                                        # Panel 4: Overall cap indices
                                        par(mar=c(1,1,2,1))
                                        plot.new()
                                        rect(0,0,1,1, col="#F8F9FA", border="#D5EEF8")
                                        idx4 <- c("Overall Cap.",
                                                  sprintf("Pp  = %s", fh(pp_h)),
                                                  sprintf("PPL = %s", fh(ppl_h)),
                                                  sprintf("PPU = %s", fh(ppu_h)),
                                                  sprintf("Ppk = %s", fh(ppk_h)),
                                                  sprintf("Cpm = %s", fh(cpm_h)))
                                        text(0.05, seq(0.92, 0.1, l=length(idx4)),
                                             idx4, adj=c(0,1), cex=0.78,
                                             font=c(2,rep(1,length(idx4)-1)),
                                             col=c("#2C3E50",rep("#E74C3C",length(idx4)-1)))

                                        # Panel 5: Within cap indices
                                        par(mar=c(1,1,2,1))
                                        plot.new()
                                        rect(0,0,1,1, col="#F8F9FA", border="#D5EEF8")
                                        idx5 <- c("Within Cap.",
                                                  sprintf("Cp  = %s", fh(cp_h)),
                                                  sprintf("CPL = %s", fh(cpl_h)),
                                                  sprintf("CPU = %s", fh(cpu_h)),
                                                  sprintf("Cpk = %s", fh(cpk_h)))
                                        text(0.05, seq(0.92, 0.1, l=length(idx5)),
                                             idx5, adj=c(0,1), cex=0.78,
                                             font=c(2,rep(1,length(idx5)-1)),
                                             col=c("#2C3E50",rep("#E74C3C",length(idx5)-1)))

                                        # Panel 6: PPM performance table
                                        par(mar=c(0.5,1,1.5,1))
                                        plot.new()
                                        rect(0,0,1,1, col="#2C3E50", border=NA)
                                        title(main="PPM Performance", col.main="white",
                                              cex.main=0.88, font.main=2, line=0.4)
                                        cols6  <- c("Metric","Observed","Exp.(Overall)","Exp.(Within)","DPMO","Yield %","Sigma Level (Wi)")
                                        xpos   <- c(0.01, 0.16, 0.32, 0.48, 0.63, 0.75, 0.87)
                                        # header
                                        for (ci in seq_along(cols6))
                                            text(xpos[ci], 0.78, cols6[ci], adj=c(0,.5),
                                                 cex=0.68, col="white", font=2)
                                        abline(h=0.68, col="grey60", lwd=0.5)
                                        # data rows
                                        rows_p <- list(
                                            c("PPM < LSL", ppm_below, exp_b_ov, exp_b_wi, "—", "—", "—"),
                                            c("PPM > USL", ppm_above, exp_a_ov, exp_a_wi, "—", "—", "—"),
                                            c("PPM Total", ppm_tot,   exp_t_ov, exp_t_wi,
                                              ppm_tot,
                                              if (is.finite(wi_ppm_num) && wi_ppm_num >= 0 && wi_ppm_num < 1e6)
                                                  sprintf("%.4f%%",(1-wi_ppm_num/1e6)*100) else "—",
                                              if (!is.na(z_st_wi_num))
                                                  sprintf("Z_st=%.2f Z_lt=%.2f",z_st_wi_num,z_lt_wi_num) else "—"))
                                        row_ys <- c(0.52, 0.34, 0.16)
                                        for (ri2 in seq_along(rows_p)) {
                                            row_col <- if(ri2==3) "white" else "#CCCCCC"
                                            row_font <- if(ri2==3) 2 else 1
                                            if(ri2==3) rect(0, 0.05, 1, 0.28, col="#34495E", border=NA)
                                            for (ci in seq_along(cols6))
                                                text(xpos[ci], row_ys[ri2], rows_p[[ri2]][ci],
                                                     adj=c(0,.5), cex=0.65, col=row_col, font=row_font)
                                        }

                                        # Overall title
                                        mtext(paste0("Process Capability Analysis — ",ri_h$varlab,
                                                     "   [", grade_h, "]"),
                                              outer=TRUE, cex=1.05, font=2, col="#2C3E50", line=2.5)
                                        mtext(sprintf("n=%d  |  Mean=%.4f  |  SD(ov)=%.5f  |  Cpk=%s  |  Ppk=%s",
                                                      n_h, xb_h, so_h, fh(cpk_h), fh(ppk_h)),
                                              outer=TRUE, cex=0.8, col="#555", line=0.8)
                                    }, error=function(e) NULL)
                                    dev.off()
                                    tryCatch(file.remove(tmpf_cap), error=function(e) NULL)
                                    dl_btn <- sprintf(
                                        '<button onclick="downloadCapCard(\'caprep_%s\',\'plt_cap_%s\',\'capability_%s\',this)" style="background:#3498DB;color:white;border:none;padding:6px 14px;border-radius:4px;font-size:11px;font-weight:bold;margin-left:8px;cursor:pointer">&#8681; Download PNG</button>',
                                        vi_h, vi_h, safe_lab)
                                }
                            }, error=function(e) invisible(NULL))
                            cap_card <- paste0(
                                sprintf('<div id="%s">',card_id),
                                # Header bar
                                '<div style="background:#2C3E50;color:white;padding:10px 16px;border-radius:4px 4px 0 0;margin:-1px -1px 0 -1px">',
                                '<table style="width:100%;border-collapse:collapse"><tr>',
                                sprintf('<td><div style="font-size:15px;font-weight:bold">Process Capability Analysis &mdash; %s</div>',ri_h$varlab),
                                sprintf('<div style="font-size:11px;opacity:0.75;margin-top:3px">%s  |  n = %d  |  Date: %s</div></td>',
                                    if(!is.null(preparedby)&&nchar(preparedby)>0) paste0("Prepared by: ",preparedby) else "",
                                    n_h, format(Sys.Date(),"%d %b %Y")),
                                sprintf('<td style="text-align:right;white-space:nowrap"><div style="background:%s;padding:8px 18px;border-radius:4px;font-weight:bold;display:inline-block">%s</div> %s</td>',
                                    grade_col, grade_h, dl_btn),
                                '</tr></table></div>',
                                # 3-column body
                                '<table style="width:100%;border-collapse:collapse;table-layout:fixed">',
                                '<tr>',
                                '<td style="width:21%;vertical-align:top;padding:8px">',left_html,'</td>',
                                '<td style="width:55%;vertical-align:top;padding:4px">',hist_div,'</td>',
                                '<td style="width:24%;vertical-align:top;padding:8px">',right_html,'</td>',
                                '</tr></table>',
                                # Bottom PPM table
                                bot_html,
                                '</div>')   # close card_id div

                            plt_divs <<- c(plt_divs, cap_card)

                            # ── Distribution Advisor ─────────────────────────
                            if (run_datadist) {
                                tryCatch({
                                    da_env <- new.env(parent=emptyenv())
                                    # Use original (pre-transform) data so gallery shows why transform was needed
                                    da_dat_arg <- if (isTRUE(ri_h$did_transform)) ri_h$orig_data else dat_h
                                    da_lsl_arg <- if (isTRUE(ri_h$did_transform)) ri_h$orig_lsl  else lsl
                                    da_usl_arg <- if (isTRUE(ri_h$did_transform)) ri_h$orig_usl  else usl
                                    da_sec <- da_html_section(
                                        da_dat_arg, ri_h$varlab, da_lsl_arg, da_usl_arg,
                                        alpha      = dist_alpha_val,
                                        include_3p = dist_3p,
                                        plt_div_fn = plt_div,
                                        out_env    = da_env,
                                        sigma_mult = sigma_mult,
                                        sigma_half = sigma_half)
                                    if (!is.null(da_sec) && nchar(da_sec) > 0)
                                        plt_divs <<- c(plt_divs, da_sec)
                                    # Output distribution-based capability as SPSS pivot table
                                    if (exists("cap_table", envir=da_env, inherits=FALSE)) {
                                        ct <- da_env$cap_table
                                        tryCatch({
                                            spsspivottable.Display(
                                                ct,
                                                title = paste0("Distribution-Based Capability: ", ri_h$varlab),
                                                templateName = "DISTCAPABILITY",
                                                outline      = "Distribution Advisor",
                                                rowdim       = "Method",
                                                coldim       = "Measure")
                                        }, error=function(e2) invisible(NULL))
                                    }
                                    # ── Full Distribution Rankings SPSS table ──────────────
                                    if (exists("dist_rankings", envir=da_env, inherits=FALSE)) {
                                        tryCatch({
                                            dr      <- da_env$dist_rankings
                                            d_alpha <- if (exists("dist_alpha", envir=da_env, inherits=FALSE)) da_env$dist_alpha else 0.05
                                            n_fits  <- length(dr)
                                            fmt_p_dr <- function(p) {
                                                if (is.na(p) || !is.finite(p)) return("—")
                                                sprintf("%.4f", p)
                                            }
                                            rank_tbl <- data.frame(
                                                Rank         = seq_len(n_fits),
                                                Distribution = sapply(dr, function(f) f$name),
                                                Params       = sapply(dr, function(f) f$nparams),
                                                `AD Stat`    = sapply(dr, function(f)
                                                                   if (is.na(f$ad)) "—" else sprintf("%.4f", f$ad)),
                                                `AD p-value` = sapply(dr, function(f) fmt_p_dr(f$ad_p)),
                                                `Pass/Fail`  = sapply(dr, function(f)
                                                                   if (is.na(f$ad_p)) "—"
                                                                   else if (f$ad_p >= d_alpha) "Pass" else "Fail"),
                                                AIC          = sapply(dr, function(f) sprintf("%.1f", f$aic)),
                                                BIC          = sapply(dr, function(f) sprintf("%.1f", f$bic)),
                                                `PP R²`      = sapply(dr, function(f)
                                                                   if (is.na(f$pp_r2)) "—" else sprintf("%.4f", f$pp_r2)),
                                                `Fitted Parameters` = sapply(dr, function(f) da_fmt_params(f)),
                                                stringsAsFactors = FALSE, check.names = FALSE
                                            )
                                            # Mark best fit
                                            rank_tbl$Rank[1] <- paste0(rank_tbl$Rank[1], " (Best)")
                                            spsspivottable.Display(
                                                rank_tbl,
                                                title        = paste0("Full Distribution Rankings — ", ri_h$varlab),
                                                templateName = "DISTRANKING",
                                                outline      = "Distribution Advisor",
                                                caption      = paste0(
                                                    "Ranked by composite score (60% AD p-value + 40% AIC rank).  ",
                                                    "AD p-values: Stephens (1974) asymptotic approximation with finite-n correction.  ",
                                                    sprintf("alpha = %.2f  |  n = %d", d_alpha, length(da_dat_arg[is.finite(da_dat_arg)]))))
                                        }, error=function(e3) invisible(NULL))
                                    }
                                }, error=function(e) invisible(NULL))
                            }
                        })
                    }

                    # ── Chart 7: Capability Sixpack (per variable) ───────────
                    for (vi_six in seq_along(results)) {
                      tryCatch({
                        rv6s  <- results[[vi_six]]
                        s6_dat  <- rv6s$data; s6_n <- rv6s$n
                        s6_xbar <- rv6s$xbar; s6_s  <- rv6s$s
                        cp_s6   <- rv6s$cp;   cpk_s6 <- rv6s$cpk
                        pp_s6   <- rv6s$pp;   ppk_s6 <- rv6s$ppk
                        ppt_s6  <- rv6s$ppm_total
                        vlab_s6 <- rv6s$varlab

                        xl6 <- range(c(s6_dat,lsl,usl),na.rm=TRUE)
                        xl6 <- xl6+diff(xl6)*0.12*c(-1,1)
                        xq6 <- seq(xl6[1],xl6[2],l=300)
                        obs6s <- seq_len(s6_n)
                        bin6  <- diff(xl6)/max(5L,ceiling(sqrt(s6_n)))
                        yl6   <- range(c(s6_dat,lsl,usl),na.rm=TRUE)
                        yl6   <- yl6+diff(yl6)*0.12*c(-1,1)

                        qq6   <- qqnorm(s6_dat,plot.it=FALSE)
                        q25_6 <- quantile(s6_dat,.25); q75_6 <- quantile(s6_dat,.75)
                        sl6   <- (q75_6-q25_6)/(qnorm(.75)-qnorm(.25))
                        in6   <- q25_6-sl6*qnorm(.25)

                        # Normality p-value for Q-Q annotation
                        s6_norm_p <- tryCatch({
                            if (exists("has_nortest") && isTRUE(has_nortest) &&
                                    requireNamespace("nortest", quietly=TRUE))
                                nortest::ad.test(s6_dat)$p.value
                            else if (s6_n >= 3L && s6_n <= 5000L)
                                shapiro.test(s6_dat)$p.value
                            else NA_real_
                        }, error=function(e) NA_real_)
                        s6_norm_nm  <- if (exists("has_nortest") && isTRUE(has_nortest) &&
                                               requireNamespace("nortest", quietly=TRUE)) "AD" else "SW"
                        s6_norm_lbl <- if (!is.na(s6_norm_p))
                                           sprintf("%s p = %.4f", s6_norm_nm, s6_norm_p)
                                       else "p = N/A"
                        s6_norm_col <- if (!is.na(s6_norm_p) && s6_norm_p > 0.05) "#1E8449" else "#C0392B"
                        s6_norm_res <- if (!is.na(s6_norm_p))
                                           if (s6_norm_p > 0.05) "  [Normal]" else "  [Non-Normal]"
                                       else ""

                        fsi <- function(v,d=4)
                            if(!is.null(v)&&length(v)==1&&!is.na(v)) formatC(v,d,format="f") else "—"
                        ttl <- function(t) list(list(x=0.5,y=1.0,xref="paper",yref="paper",
                            text=paste0("<b>",t,"</b>"),showarrow=FALSE,yanchor="bottom",
                            font=list(size=11,color="#2C3E50")))

                        sh_hl6 <- Filter(Negate(is.null), list(
                            list(type="line",x0=1,x1=s6_n,y0=s6_xbar,y1=s6_xbar,
                                xref="x",yref="y",line=list(color="#27AE60",width=1.5,dash="dash")),
                            if(!is.null(lsl)) list(type="line",x0=1,x1=s6_n,y0=lsl,y1=lsl,
                                xref="x",yref="y",name="spec_limit",line=list(color="#E74C3C",width=1.5,dash="dot")),
                            if(!is.null(usl)) list(type="line",x0=1,x1=s6_n,y0=usl,y1=usl,
                                xref="x",yref="y",name="spec_limit",line=list(color="#E74C3C",width=1.5,dash="dot"))
                        ))
                        sh_vl6 <- Filter(Negate(is.null), list(
                            if(!is.null(lsl)) list(type="line",x0=lsl,x1=lsl,y0=0,y1=1,
                                yref="paper",xref="x",name="spec_limit",line=list(color="#E74C3C",width=1.5,dash="dash")),
                            if(!is.null(usl)) list(type="line",x0=usl,x1=usl,y0=0,y1=1,
                                yref="paper",xref="x",name="spec_limit",line=list(color="#E74C3C",width=1.5,dash="dash"))
                        ))
                        sh_bh6 <- Filter(Negate(is.null), list(
                            if(!is.null(lsl)) list(type="line",x0=0,x1=1,xref="paper",
                                y0=lsl,y1=lsl,yref="y",name="spec_limit",line=list(color="#E74C3C",width=1.5,dash="dot")),
                            if(!is.null(usl)) list(type="line",x0=0,x1=1,xref="paper",
                                y0=usl,y1=usl,yref="y",name="spec_limit",line=list(color="#E74C3C",width=1.5,dash="dot"))
                        ))

                        # Panel 1: Run chart
                        sp6_run <- plot_ly(x=obs6s,y=s6_dat,type="scatter",
                            mode="lines+markers",showlegend=FALSE,
                            marker=list(color=pcol(vi_six),size=4),
                            line=list(color=pcol(vi_six),width=1.2))
                        sp6_run <- layout(sp6_run,
                            yaxis=list(title="Value",range=yl6,titlefont=list(size=10)),
                            xaxis=list(title="Obs"),
                            shapes=sh_hl6,annotations=ttl("Run Chart"))

                        # Panel 2: Histogram (pre-computed to avoid histnorm/xbins warnings)
                        brks6   <- seq(xl6[1], xl6[2], by=bin6)
                        hh6     <- hist(s6_dat, breaks=brks6, plot=FALSE)
                        sp6_hist <- plot_ly(x=hh6$mids, y=hh6$density, type="bar",
                            showlegend=FALSE, width=bin6*0.9,
                            marker=list(color="#AED6F1",line=list(color="white",width=0.4)))
                        sp6_hist <- add_trace(sp6_hist,x=xq6,y=dnorm(xq6,s6_xbar,s6_s),
                            type="scatter",mode="lines",showlegend=FALSE,
                            line=list(color="#2980B9",width=2))
                        sp6_hist <- layout(sp6_hist,
                            xaxis=list(title="Value",range=xl6),
                            yaxis=list(title="Density",titlefont=list(size=10)),
                            shapes=sh_vl6,annotations=ttl("Histogram"))

                        # Panel 3: Normal Q-Q
                        sp6_qq <- plot_ly(x=qq6$x,y=qq6$y,type="scatter",mode="markers",
                            showlegend=FALSE,marker=list(color="#2980B9",size=4,opacity=0.7))
                        sp6_qq <- add_trace(sp6_qq,x=range(qq6$x),
                            y=in6+sl6*range(qq6$x),type="scatter",mode="lines",
                            showlegend=FALSE,line=list(color="#E74C3C",width=2,dash="dot"))
                        sp6_qq <- layout(sp6_qq,
                            xaxis=list(title="Theoretical"),
                            yaxis=list(title="Sample",titlefont=list(size=10)),
                            annotations=list(list(
                                x=0.5, y=1.0, xref="paper", yref="paper",
                                text=paste0(
                                    "<b>Normal Q-Q</b>  ",
                                    "<span style='font-size:9px;color:", s6_norm_col, "'>",
                                    s6_norm_lbl, s6_norm_res, "</span>"
                                ),
                                showarrow=FALSE, yanchor="bottom",
                                font=list(size=11, color="#2C3E50"))))

                        # Panel 4: Capability density
                        sp6_cap <- plot_ly(x=xq6,y=dnorm(xq6,s6_xbar,s6_s),
                            type="scatter",mode="lines",showlegend=FALSE,
                            line=list(color="#2980B9",width=2),
                            fill="tozeroy",fillcolor="#D6EAF888")
                        sp6_cap <- layout(sp6_cap,
                            xaxis=list(title="Value",range=xl6),
                            yaxis=list(title="Density",titlefont=list(size=10)),
                            shapes=sh_vl6,annotations=ttl("Capability"))

                        # Panel 5: Box plot
                        sp6_box <- plot_ly(y=s6_dat,type="box",name=vlab_s6,
                            marker=list(color=pcol(vi_six),size=4,outliercolor="#E74C3C"),
                            fillcolor=paste0(substr(pcol(vi_six),1,7),"55"),
                            line=list(color=pcol(vi_six)),
                            boxmean=TRUE,showlegend=FALSE)
                        sp6_box <- layout(sp6_box,
                            yaxis=list(title="Value",range=yl6,titlefont=list(size=10)),
                            shapes=sh_bh6,annotations=ttl("Box Plot"))

                        # Panel 6: Indices (text — avoids table-in-subplot bug)
                        idx_txt6 <- paste(c(
                            paste0("<b>",vlab_s6,"</b>"),
                            sprintf("n = %d  |  Mean = %.4f",s6_n,s6_xbar),
                            sprintf("SD = %.5f",s6_s),"",
                            paste0("Cp  = ",fsi(cp_s6)),
                            paste0("Cpk = ",fsi(cpk_s6)),
                            paste0("Pp  = ",fsi(pp_s6)),
                            paste0("Ppk = ",fsi(ppk_s6)),
                            paste0("PPM = ",fsi(ppt_s6,d=1))
                        ),collapse="<br>")
                        sp6_idx <- plot_ly(x=c(0.5),y=c(0.5),type="scatter",mode="markers",
                            marker=list(size=0,color="white"),showlegend=FALSE)
                        sp6_idx <- layout(sp6_idx,
                            xaxis=list(visible=FALSE,range=c(0,1)),
                            yaxis=list(visible=FALSE,range=c(0,1)),
                            annotations=c(ttl("Indices"),list(list(
                                x=0.5,y=0.42,xref="x",yref="y",
                                text=idx_txt6,showarrow=FALSE,
                                xanchor="center",yanchor="middle",
                                font=list(size=13,color="#2C3E50"),
                                bgcolor="#F8F9FA",bordercolor="#BDC3C7",borderwidth=1))))

                        # ── Pre-render all 6 sixpack panels as ONE static PNG for download ──
                        # (the interactive panels are separate Plotly divs with their modebar
                        # disabled, so the camera icon isn't available per-panel; this gives
                        # users a single combined image instead of just one panel.)
                        six_dl_btn <- ""
                        tryCatch({
                            if (requireNamespace("base64enc", quietly=TRUE)) {
                                tmpf_s6 <- tempfile(fileext=".png")
                                png(tmpf_s6, width=1200, height=1500, res=120, bg="white")
                                tryCatch({
                                    par(mfrow=c(3,2), mar=c(3.5,3.5,2.5,1.5),
                                        oma=c(0,0,3,0), cex.lab=0.95, cex.axis=0.85)
                                    nbr6s <- max(5L, ceiling(sqrt(s6_n)))

                                    # 1 — Run Chart
                                    plot(obs6s, s6_dat, type="o", pch=19, col=pcol(vi_six),
                                         cex=0.5, lwd=1.2, main="Run Chart", xlab="Obs", ylab="Value")
                                    abline(h=s6_xbar, col="#27AE60", lwd=1.5); draw_spec_h()
                                    grid(col="grey92", lty=1, lwd=0.4)

                                    # 2 — Histogram
                                    hist(s6_dat, breaks=nbr6s, freq=FALSE, col="#AED6F1", border="white",
                                         main="Histogram", xlab="Value", ylab="Density", xlim=xl6)
                                    lines(xq6, dnorm(xq6, s6_xbar, s6_s), col=pcol(vi_six), lwd=2)
                                    draw_spec_v()

                                    # 3 — Normal Q-Q
                                    qqnorm(s6_dat,
                                           main=paste0("Normal Q-Q\n",
                                                        s6_norm_lbl, s6_norm_res),
                                           pch=19, col=pcol(vi_six), cex=0.55,
                                           cex.main=0.85)
                                    qqline(s6_dat, col="#E74C3C", lwd=2)
                                    grid(col="grey92",lty=1,lwd=0.4)

                                    # 4 — Capability Distribution
                                    dy6 <- dnorm(xq6, s6_xbar, s6_s)
                                    plot(xq6, dy6, type="l", col=pcol(vi_six), lwd=2,
                                         main="Capability", xlab="Value", ylab="Density", xlim=xl6)
                                    polygon(c(xq6,rev(xq6)), c(dy6,rep(0,length(xq6))),
                                            col=paste0(substr(pcol(vi_six),1,7),"33"), border=NA)
                                    lines(xq6, dy6, col=pcol(vi_six), lwd=2)
                                    draw_spec_v()
                                    if (!is.null(cpk_s6) && !is.na(cpk_s6))
                                        legend("topright", legend=sprintf("Cpk = %.3f",cpk_s6),
                                               bty="o", bg="#FFFFF0", cex=0.8, box.lwd=0.5)

                                    # 5 — Box Plot
                                    boxplot(s6_dat, col="#AED6F1", border="#2C3E50",
                                            main="Box Plot", ylab="Value",
                                            pch=19, outcex=0.7, outcol="#E74C3C")
                                    draw_spec_h(); grid(col="grey92",lty=1,lwd=0.4)

                                    # 6 — Statistics panel
                                    plot.new(); plot.window(xlim=c(0,1), ylim=c(0,1))
                                    title(main="Indices", font.main=2, cex.main=0.95)
                                    rect(0,0,1,1,col="#FAFAFA",border="#BDC3C7")
                                    sp6_lines <- c(
                                        sprintf("n   = %d",    s6_n),
                                        sprintf("Mean= %.4f",  s6_xbar),
                                        sprintf("s   = %.4f",  s6_s),
                                        "",
                                        paste0("Cp  = ", safe_idx("", cp_s6)),
                                        paste0("Cpk = ", safe_idx("", cpk_s6)),
                                        paste0("Pp  = ", safe_idx("", pp_s6)),
                                        paste0("Ppk = ", safe_idx("", ppk_s6)),
                                        "",
                                        paste0("PPM = ", safe_idx("", ppt_s6, fmt="%.1f"))
                                    )
                                    text(0.5, 0.95, paste(sp6_lines, collapse="\n"),
                                         cex=0.78, adj=c(0.5,1), col="#2C3E50")

                                    if (!is.null(logo_img)) tryCatch(
                                        grid::grid.raster(logo_img, x=0.97, y=0.97,
                                            width=grid::unit(0.07,"npc"), height=grid::unit(0.05,"npc"),
                                            just=c("right","top")),
                                        error=function(e) invisible(NULL))
                                    mtext(paste0("Capability Sixpack — ", vlab_s6),
                                          outer=TRUE, cex=1.1, font=2, col="#2C3E50")
                                }, error=function(e) NULL)
                                dev.off()
                                b64_s6 <- base64enc::base64encode(tmpf_s6)
                                tryCatch(file.remove(tmpf_s6), error=function(e) NULL)
                                # six_dl_btn assigned below after six_ids_js is defined
                            }
                        }, error=function(e) invisible(NULL))

                        # CSS grid of 6 individual charts — avoids subplot domain squishing
                        sp_cfg <- '{"responsive":true,"displayModeBar":false,"displaylogo":false}'
                        # Smaller logo sized for the ~230px sixpack panels (baked into each
                        # panel's layout.images so it travels with the figure, same as plt_div)
                        sp_logo <- if (nchar(logo_b64_src) > 0) list(list(
                            source=logo_b64_src,
                            xref="paper", yref="paper",
                            x=1.0, y=1.0,
                            sizex=0.16, sizey=0.13,
                            xanchor="right", yanchor="top",
                            layer="above"
                        )) else list()
                        mk_sp <- function(p, id, ht=230) {
                            if (length(sp_logo)) p <- plotly::layout(p, images = sp_logo)
                            fj <- tryCatch(suppressMessages(suppressWarnings(plotly::plotly_json(p, FALSE))),
                                           error=function(e) "{}")
                            sprintf('<div style="position:relative;min-width:0;background:#FAFAFA;border:1px solid #ECF0F1;border-radius:4px"><div id="%s" style="width:100%%;height:%dpx"></div><script>Plotly.react("%s",%s,{},%s)</script></div>',
                                    id, ht, id, fj, sp_cfg)
                        }
                        sid <- function(nm) paste0("sp6", nm, "_", vi_six)
                        six_ids <- c(sid("run"),sid("hist"),sid("qq"),sid("cap"),sid("box"),sid("idx"))
                        six_ids_js <- paste0("['", paste(six_ids, collapse="','"), "']")
                        six_dl_btn <- sprintf(
                            '<button onclick="downloadSixpack(%s,\'capability_sixpack_%s\')" style="background:#3498DB;color:white;border:none;padding:6px 14px;border-radius:4px;font-size:11px;font-weight:bold;margin-left:8px;cursor:pointer">&#8681; Download PNG</button>',
                            six_ids_js, gsub("[^A-Za-z0-9_]","_",vlab_s6))
                        six_grid_html <- paste(c(
                            mk_sp(sp6_run,  six_ids[1]),
                            mk_sp(sp6_hist, six_ids[2]),
                            mk_sp(sp6_qq,   six_ids[3]),
                            mk_sp(sp6_cap,  six_ids[4]),
                            mk_sp(sp6_box,  six_ids[5]),
                            mk_sp(sp6_idx,  six_ids[6])
                        ), collapse="\n")
                        # Group theme picker for sixpack (controls all 6 panels)
                        six_panel_id <- paste0("thm_sp6_", vi_six)
                        sw6 <- function(nm, lbl, c1, c2) sprintf(
                            '<div title="%s" onclick="applyThemeGroup(%s,\'%s\')" style="background:linear-gradient(135deg,%s 50%%,%s 50%%);width:26px;height:26px;border-radius:4px;cursor:pointer;border:2px solid transparent;flex-shrink:0" onmouseover="this.style.borderColor=\'#333\'" onmouseout="this.style.borderColor=\'transparent\'"></div>',
                            lbl, six_ids_js, nm, c1, c2)
                        six_swatches <- paste0(
                            sw6("blue","Ocean Blue","#AED6F1","#2C3E50"),
                            sw6("teal","Forest",    "#A8E6CF","#006064"),
                            sw6("warm","Warm",      "#FFCCBC","#E65100"),
                            sw6("dark","Dark",      "#546E7A","#1A237E"),
                            sw6("gray","Grayscale", "#CFD8DC","#37474F"))
                        six_theme_panel <- sprintf(
                            '<div id="%s" style="display:none;position:absolute;top:100%%;left:0;z-index:50;background:white;border:1px solid #ccc;border-radius:6px;padding:10px 12px;box-shadow:0 4px 14px rgba(0,0,0,0.18);min-width:188px;margin-top:4px"><div style="font-size:11px;font-weight:bold;color:#555;margin-bottom:8px;letter-spacing:.5px">COLOR THEME</div><div style="display:flex;gap:7px;align-items:center">%s</div><div style="font-size:10px;color:#999;margin-top:7px">Blue &bull; Forest &bull; Warm &bull; Dark &bull; Gray</div></div>',
                            six_panel_id, six_swatches)
                        six_spec_btn_id <- paste0("specbtn_sp6_", vi_six)
                        six_spec_btn <- if (has_specs) sprintf(
                            '<button id="%s" onclick="toggleSixpackSpec(%s,\'%s\')" title="Show/hide specification limits (LSL/USL)" style="background:#8E44AD;color:white;border:1px solid #ccc;border-radius:4px;height:26px;padding:0 9px;cursor:pointer;font-size:10px;font-weight:bold;margin-left:8px;vertical-align:middle">LSL/USL</button>',
                            six_spec_btn_id, six_ids_js, six_spec_btn_id) else
                        sprintf('<button id="%s" disabled title="No specification limits entered" style="background:rgba(142,68,173,0.25);color:#555;border:1px solid #ccc;border-radius:4px;height:26px;padding:0 9px;cursor:not-allowed;font-size:10px;font-weight:bold;margin-left:8px;vertical-align:middle;opacity:0.55">LSL/USL</button>',
                            six_spec_btn_id)
                        six_gear <- sprintf(
                            '<div style="position:relative;display:inline-block;vertical-align:middle"><button onclick="toggleThemePanel(event,\'%s\')" title="Color theme" style="background:rgba(255,255,255,0.92);border:1px solid #ccc;border-radius:4px;width:26px;height:26px;cursor:pointer;font-size:14px;line-height:1;padding:0;margin-left:8px;vertical-align:middle">&#9881;</button>%s</div>%s',
                            six_panel_id, six_theme_panel, six_spec_btn)
                        plt_divs <- c(plt_divs, sprintf(
                            '<div style="margin:28px 0 8px 0"><div style="display:flex;align-items:center;margin-bottom:10px"><h3 style="color:#2C3E50;margin:0;font-size:15px;border-left:4px solid #3498DB;padding-left:10px;flex:1">Capability Sixpack — %s</h3>%s%s</div><div style="display:grid;grid-template-columns:1fr 1fr;gap:6px">%s</div></div>',
                            vlab_s6, six_gear, six_dl_btn, six_grid_html))
                      }, error=function(e_s6)
                          warns$warn(paste("Sixpack error (",vi_six,"):",e_s6$message)))
                    }

                    # ── Assemble HTML page ────────────────────────────────────
                    tbl_rows <- paste(sapply(seq_along(results), function(vi) {
                        ri  <- results[[vi]]
                        fv  <- function(v, d=4)
                            if (!is.null(v)&&length(v)==1&&!is.na(v))
                                formatC(v, d, format="f") else "&mdash;"
                        cap_col <- function(v) {
                            if (is.null(v)||is.na(v)) return("#888")
                            if (v>=1.33) "#27AE60" else if (v>=1.0) "#E67E22" else "#E74C3C"
                        }
                        col_cp  <- cap_col(ri$cp);  col_cpk <- cap_col(ri$cpk)
                        col_pp  <- cap_col(ri$pp);  col_ppk <- cap_col(ri$ppk)
                        sprintf(
                            '<tr><td><b>%s</b></td><td>%d</td>
<td>%.4f</td><td>%.5f</td>
<td style="color:%s;font-weight:bold">%s</td>
<td style="color:%s;font-weight:bold">%s</td>
<td style="color:%s;font-weight:bold">%s</td>
<td style="color:%s;font-weight:bold">%s</td>
<td>%.1f</td></tr>',
                            ri$varlab, ri$n, ri$xbar, ri$s,
                            col_cp, fv(ri$cp), col_cpk, fv(ri$cpk),
                            col_pp, fv(ri$pp), col_ppk, fv(ri$ppk),
                            if (!is.null(ri$ppm_total)&&!is.na(ri$ppm_total))
                                ri$ppm_total else 0)
                    }), collapse="\n")

                    # Logo (base64 for header image)
                    logo_html_str <- ""
                    if (!is.null(logo_img)) tryCatch({
                        if (requireNamespace("base64enc", quietly=TRUE)) {
                            tmpf_l <- tempfile(fileext=".png")
                            png(tmpf_l, width=200, height=70, bg="transparent")
                            tryCatch({
                                op_l <- par(mar=c(0,0,0,0)); plot.new()
                                ih_l <- if(is.array(logo_img)) dim(logo_img)[1] else nrow(logo_img)
                                iw_l <- if(is.array(logo_img)) dim(logo_img)[2] else ncol(logo_img)
                                rasterImage(logo_img,0,0,1,1)
                            }, error=function(e) NULL); dev.off()
                            b64_l <- base64enc::base64encode(tmpf_l)
                            logo_html_str <- sprintf(
                                '<img src="data:image/png;base64,%s" style="height:52px;vertical-align:middle;margin-right:16px">',
                                b64_l)
                        }
                    }, error=function(e) invisible(NULL))

                    gen_ts  <- format(Sys.time(), "%Y-%m-%d %H:%M")
                    spec_ts <- paste(Filter(nchar, c(
                        if (!is.null(lsl))    sprintf("LSL = %.4f", lsl),
                        if (!is.null(usl))    sprintf("USL = %.4f", usl),
                        if (!is.null(target)) sprintf("Target = %.4f", target)
                    )), collapse="  |  ")
                    var_ts  <- paste(sapply(results, `[[`, "varlab"), collapse=", ")

                    charts_html <- paste(
                        sapply(plt_divs,
                               function(d) sprintf('<div class="card">%s</div>', d)),
                        collapse="\n")

                    html_tpl <- '<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width,initial-scale=1">
<title>Process Capability Report</title>
<script src="https://cdn.plot.ly/plotly-2.35.0.min.js" charset="utf-8"></script>
<script src="https://cdnjs.cloudflare.com/ajax/libs/html2canvas/1.4.1/html2canvas.min.js"></script>
<style>
*{box-sizing:border-box}
body{font-family:Arial,Helvetica,sans-serif;margin:0;padding:0;background:#F0F2F5;color:#2C3E50}
.hdr{background:linear-gradient(135deg,#1a2636 0%,#2C3E50 60%,#34495E 100%);color:white;padding:18px 40px;display:flex;align-items:center;box-shadow:0 3px 10px rgba(0,0,0,.3)}
.hdr h1{margin:0;font-size:21px;font-weight:700;letter-spacing:.3px}
.hdr .meta{font-size:11.5px;opacity:.75;margin-top:5px}
.body{max-width:1350px;margin:0 auto;padding:28px 20px}
.info-bar{background:white;border-radius:8px;padding:12px 20px;margin-bottom:22px;box-shadow:0 1px 5px rgba(0,0,0,.08);display:flex;gap:28px;font-size:13px;flex-wrap:wrap}
.info-bar span b{color:#2C3E50}
h2{color:#2C3E50;border-bottom:3px solid #3498DB;padding-bottom:6px;margin:30px 0 14px;font-size:18px}
h3{color:#2C3E50;margin:0 0 6px 0;font-size:15px;border-left:4px solid #3498DB;padding-left:10px}
.card{background:white;border-radius:10px;padding:18px 18px 10px;margin-bottom:22px;box-shadow:0 2px 8px rgba(0,0,0,.07)}
table{border-collapse:collapse;width:100%;font-size:13px}
thead tr{background:#2C3E50;color:white}
thead th{padding:9px 12px;text-align:right;font-weight:600}
thead th:first-child{text-align:left}
tbody tr:nth-child(even){background:#F8F9FA}
tbody tr:hover{background:#EBF5FB}
tbody td{padding:8px 12px;border-bottom:1px solid #EEE;text-align:right}
tbody td:first-child{text-align:left}
.tip{font-size:12.5px;color:#666;margin:0 0 18px;font-style:italic}
.footer{text-align:center;font-size:11px;color:#aaa;padding:24px;margin-top:8px}
</style>
</head>
<body>
<div class="hdr">
  {{LOGO}}
  <div><h1>Process Capability Analysis Report</h1>
  <div class="meta">{{COMPANY}} &nbsp;|&nbsp; Generated: {{GENTS}} &nbsp;|&nbsp; {{SPECOR}}</div></div>
</div>
<div class="body">
  <div class="info-bar">
    <span><b>Sigma Multiplier:</b> {{SIGMA}}</span>
    <span><b>Spec Limits:</b> {{SPECTS}}</span>
    <span><b>Variables:</b> {{VARTS}}</span>
    <span><b>Prepared by:</b> {{PREPBY}}</span>
    <span><b>Reviewed by:</b> {{REVBY}}</span>
  </div>

  <h2>Summary Table</h2>
  <div class="card">
  <table>
    <thead><tr>
      <th style="text-align:left">Variable</th><th>n</th><th>Mean</th><th>StdDev</th>
      <th>Cp</th><th>Cpk</th><th>Pp</th><th>Ppk</th><th>PPM Total</th>
    </tr></thead>
    <tbody>{{TBLROWS}}</tbody>
  </table>
  </div>

  <h2>Interactive Charts</h2>
  <p class="tip">All charts are interactive &mdash; zoom with scroll/drag, pan, hover for values, click legend items to toggle series. Download any chart as SVG via the camera icon in the toolbar.</p>
  {{CHARTS}}

  <div class="footer">Process Capability Analysis &mdash; Generated {{GENTS}}</div>
</div>
<style>
/* Hide Plotly.js watermark link completely */
.modebar-group a[data-title*="Plotly"],
.modebar-group a[href*="plotly"],
.js-plotly-plot .plotly .modebar .modebar-group:last-child { display:none!important; }
</style>
<script>
// ── Resize trigger: resize every plotly chart directly (more reliable than window.resize)
window.addEventListener("load", function() {
    function resizeAll() {
        document.querySelectorAll(".js-plotly-plot").forEach(function(el) {
            try { Plotly.Plots.resize(el); } catch(e) {}
        });
    }
    setTimeout(resizeAll, 350);
    setTimeout(resizeAll, 900);
    setTimeout(resizeAll, 1800);
});

// ── Theme definitions ─────────────────────────────────────────────────────
var CHART_THEMES = {
    blue: { spec:"#E74C3C", line1:"#2C3E50", line2:"#E74C3C",
            bar:"#AED6F1", bar_border:"#7FB3D3",
            palette:["#2980B9","#E74C3C","#27AE60","#8E44AD","#F39C12","#16A085","#D35400","#2C3E50"] },
    teal: { spec:"#E64A19", line1:"#00695C", line2:"#E64A19",
            bar:"#A8E6CF", bar_border:"#70C99A",
            palette:["#00897B","#E53935","#43A047","#8E24AA","#FB8C00","#039BE5","#C0CA33","#546E7A"] },
    warm: { spec:"#BF360C", line1:"#4A148C", line2:"#BF360C",
            bar:"#FFCCBC", bar_border:"#FF8A65",
            palette:["#F4511E","#8E24AA","#F9A825","#00ACC1","#7CB342","#D81B60","#6D4C41","#607D8B"] },
    dark: { spec:"#FF7043", line1:"#4DD0E1", line2:"#FF7043",
            bar:"#546E7A", bar_border:"#37474F",
            palette:["#29B6F6","#EF5350","#66BB6A","#CE93D8","#FFCA28","#4DD0E1","#FF8A65","#90A4AE"] },
    gray: { spec:"#757575", line1:"#37474F", line2:"#B71C1C",
            bar:"#CFD8DC", bar_border:"#B0BEC5",
            palette:["#546E7A","#78909C","#90A4AE","#455A64","#607D8B","#263238","#B0BEC5","#37474F"] }
};

function toggleThemePanel(evt, panelId) {
    evt.stopPropagation();
    var p = document.getElementById(panelId);
    if (!p) return;
    var isOpen = p.style.display !== "none";
    document.querySelectorAll("[id^=\'thm_\']").forEach(function(el){ el.style.display="none"; });
    if (!isOpen) p.style.display = "block";
}
document.addEventListener("click", function() {
    document.querySelectorAll("[id^=\'thm_\']").forEach(function(el){ el.style.display="none"; });
});

// Toggle visibility of layout shapes/annotations tagged with a given group
// name (we stamp control-limit lines with name="ctrl_limit" when building
// the figure). Lets the user show/hide UCL/LCL bands without affecting the
// always-on center-line / spec-limit references.
var CTRL_TOGGLE_STATE = {};
function toggleCtrlLimits(divId, btnId) {
    var gd = document.getElementById(divId);
    if (!gd || !gd.layout) return;
    var on = CTRL_TOGGLE_STATE[divId] !== false;   // default ON
    var nextOn = !on;
    CTRL_TOGGLE_STATE[divId] = nextOn;
    var upd = {};
    if (gd.layout.shapes) {
        upd.shapes = gd.layout.shapes.map(function(s){
            if (s.name === "ctrl_limit") {
                var s2 = Object.assign({}, s); s2.visible = nextOn; return s2;
            }
            return s;
        });
    }
    if (gd.layout.annotations) {
        upd.annotations = gd.layout.annotations.map(function(a){
            if (a.name === "ctrl_limit") {
                var a2 = Object.assign({}, a); a2.visible = nextOn; return a2;
            }
            return a;
        });
    }
    Plotly.relayout(divId, upd);
    var btn = document.getElementById(btnId);
    if (btn) {
        btn.style.background = nextOn ? "#3498DB" : "rgba(255,255,255,0.92)";
        btn.style.color = nextOn ? "white" : "#555";
    }
}

// Show/hide spec limits (LSL/USL) — mirrors toggleCtrlLimits but targets
// shapes/annotations with name="spec_limit" AND traces whose names contain
// "[spec_limit]" (violin/box dummy legend traces are tagged this way).
// Cache for removed spec_limit shapes/annotations (remove-and-restore approach
// ensures PNG camera download also reflects the hidden state — visibility:false
// on shapes does not propagate reliably to Plotly\'s static image renderer).
var SPEC_TOGGLE_STATE = {};
var SPEC_SH_CACHE  = {};
var SPEC_ANN_CACHE = {};

function _applySpecToggle(divId, nextOn) {
    var gd = document.getElementById(divId);
    if (!gd || !gd.layout) return;
    var upd = {};
    var shapes = gd.layout.shapes || [];
    var annots = gd.layout.annotations || [];
    if (!nextOn) {
        // Hide: remove spec_limit shapes/annotations and cache them
        SPEC_SH_CACHE[divId]  = shapes.filter(function(s){ return s.name === "spec_limit"; });
        SPEC_ANN_CACHE[divId] = annots.filter(function(a){ return a.name === "spec_limit"; });
        upd.shapes      = shapes.filter(function(s){ return s.name !== "spec_limit"; });
        upd.annotations = annots.filter(function(a){ return a.name !== "spec_limit"; });
    } else {
        // Show: restore cached shapes/annotations
        var cachedSh  = SPEC_SH_CACHE[divId]  || [];
        var cachedAnn = SPEC_ANN_CACHE[divId] || [];
        upd.shapes      = shapes.filter(function(s){ return s.name !== "spec_limit"; }).concat(cachedSh);
        upd.annotations = annots.filter(function(a){ return a.name !== "spec_limit"; }).concat(cachedAnn);
    }
    Plotly.relayout(divId, upd);
    // Toggle dummy legend traces tagged "[spec_limit]"
    var traceIdx = [];
    if (gd.data) gd.data.forEach(function(tr, i){
        if (tr.name && tr.name.indexOf("[spec_limit]") >= 0) traceIdx.push(i);
    });
    if (traceIdx.length > 0)
        Plotly.restyle(divId, {visible: nextOn ? true : "legendonly"}, traceIdx);
}

function toggleSpecLimits(divId, btnId) {
    var on = SPEC_TOGGLE_STATE[divId] !== false;
    var nextOn = !on;
    SPEC_TOGGLE_STATE[divId] = nextOn;
    _applySpecToggle(divId, nextOn);
    var btn = document.getElementById(btnId);
    if (btn) {
        btn.style.background = nextOn ? "#8E44AD" : "rgba(255,255,255,0.92)";
        btn.style.color = nextOn ? "white" : "#555";
    }
}

// Sixpack variant: toggle spec limits across all 6 panels at once
var SIX_SPEC_STATE = {};
function toggleSixpackSpec(divIds, btnId) {
    var key = btnId;
    var on = SIX_SPEC_STATE[key] !== false;
    var nextOn = !on;
    SIX_SPEC_STATE[key] = nextOn;
    divIds.forEach(function(divId){ _applySpecToggle(divId, nextOn); });
    var btn = document.getElementById(btnId);
    if (btn) {
        btn.style.background = nextOn ? "#8E44AD" : "rgba(255,255,255,0.92)";
        btn.style.color = nextOn ? "white" : "#555";
    }
}

// Download the full per-variable capability card as PNG.
// Rasterises Plotly histogram (respects current toggle/LSL-USL state) and
// composites with the rest of the card via html2canvas.
function downloadCapCard(cardId, histDivId, filename, btn) {
    var gd = document.getElementById(histDivId);
    if (!gd) { return; }
    if (btn) btn.style.visibility = \'hidden\';
    var W = gd.offsetWidth || 520, H = gd.offsetHeight || 380;
    Plotly.toImage(gd, {format: \'png\', scale: 2, width: W, height: H}).then(function(dataUrl) {
        var overlay = document.createElement(\'img\');
        overlay.src = dataUrl;
        overlay.id = \'_cap_dl_overlay_\';
        overlay.style.cssText = \'position:absolute;top:0;left:0;width:\' + W + \'px;height:\' + H + \'px;z-index:9999;pointer-events:none\';
        gd.style.position = \'relative\';
        gd.appendChild(overlay);
        var card = document.getElementById(cardId);
        return html2canvas(card, {scale: 2, backgroundColor: \'#ffffff\', logging: false, useCORS: true}).then(function(canvas) {
            if (gd.contains(overlay)) gd.removeChild(overlay);
            if (btn) btn.style.visibility = \'\';
            var link = document.createElement(\'a\');
            link.download = filename + \'.png\';
            link.href = canvas.toDataURL(\'image/png\');
            document.body.appendChild(link);
            link.click();
            document.body.removeChild(link);
        }).catch(function() {
            if (gd.contains(overlay)) gd.removeChild(overlay);
            if (btn) btn.style.visibility = \'\';
            Plotly.downloadImage(gd, {format: \'png\', filename: filename, scale: 2});
        });
    }).catch(function() {
        if (btn) btn.style.visibility = \'\';
        Plotly.downloadImage(gd, {format: \'png\', filename: filename, scale: 2});
    });
}

// Download all 6 sixpack panels as a single stitched PNG, reflecting current
// toggle state (uses Plotly.toImage so hidden shapes/lines are excluded).
function downloadSixpack(divIds, filename) {
    var W = 560, H = 320, COLS = 2;
    var canvas = document.createElement(\'canvas\');
    canvas.width  = W * COLS * 2;
    canvas.height = H * Math.ceil(divIds.length / COLS) * 2;
    var ctx = canvas.getContext(\'2d\');
    ctx.fillStyle = \'#ffffff\';
    ctx.fillRect(0, 0, canvas.width, canvas.height);
    var idx = 0;
    function nextPanel() {
        if (idx >= divIds.length) {
            var link = document.createElement(\'a\');
            link.download = filename + \'.png\';
            link.href = canvas.toDataURL(\'image/png\');
            document.body.appendChild(link);
            link.click();
            document.body.removeChild(link);
            return;
        }
        var i = idx++;
        var col = i % COLS, row = Math.floor(i / COLS);
        Plotly.toImage(divIds[i], {format: \'png\', scale: 2, width: W, height: H}).then(function(dataUrl) {
            var img = new Image();
            img.onload = function() {
                ctx.drawImage(img, col * W * 2, row * H * 2, W * 2, H * 2);
                nextPanel();
            };
            img.src = dataUrl;
        }).catch(function() { nextPanel(); });
    }
    nextPanel();
}

// Map a trace name to a fixed "role" color, or null if it should get a palette color
function roleColor(nm, t) {
    if (nm.indexOf("within") >= 0 || nm.indexOf("cp") >= 0 || nm === "data" || nm === "kde") return t.line1;
    if (nm.indexOf("overall") >= 0 || nm.indexOf("pp") >= 0) return t.line2;
    if (nm.indexOf("spec") >= 0 || nm.indexOf("lsl") >= 0 || nm.indexOf("usl") >= 0 ||
        nm.indexOf("limit") >= 0 || nm.indexOf("ucl") >= 0 || nm.indexOf("lcl") >= 0) return t.spec;
    if (nm.indexOf("mean") >= 0 || nm.indexOf("target") >= 0 || nm.indexOf("centerline") >= 0 ||
        nm.indexOf("cl ") >= 0 || nm === "cl" || nm.indexOf("xbar") >= 0) return "#27AE60";
    if (nm.indexOf("reference") >= 0 || nm.indexOf("normal") >= 0 || nm.indexOf("45") >= 0) return t.spec;
    return null;
}

function hexAlpha(hex, alpha) {
    if (!hex || hex.charAt(0) !== "#") return hex;
    return hex.length === 7 ? (hex + alpha) : hex;
}

function applyTheme(divId, themeName) {
    var t = CHART_THEMES[themeName];
    if (!t) return;
    var gd = document.getElementById(divId);
    if (!gd || !gd.data) return;
    var traces = gd.data;
    var pk = 0;   // palette cursor — advances only for "unnamed"/series traces

    traces.forEach(function(tr, i) {
        var nm = (tr.name || "").toLowerCase();
        var role = roleColor(nm, t);
        var update = {};
        var ty = tr.type || "scatter";

        if (ty === "bar" || ty === "histogram") {
            var c = role || t.palette[pk % t.palette.length];
            if (!role) pk++;
            update["marker.color"] = c;
            update["marker.line.color"] = role ? t.bar_border : c;
            update["opacity"] = 0.65;

        } else if (ty === "box" || ty === "violin") {
            var c = role || t.palette[pk % t.palette.length];
            if (!role) pk++;
            update["marker.color"] = c;
            update["line.color"]   = c;
            update["fillcolor"]    = hexAlpha(c, "55");

        } else if (ty === "scatter" || ty === "scattergl") {
            var mode = tr.mode || "lines";
            var c = role || t.palette[pk % t.palette.length];
            if (!role && (mode.indexOf("lines") >= 0 || mode.indexOf("markers") >= 0)) pk++;
            if (mode.indexOf("lines") >= 0) {
                update["line.color"] = c;
                if (tr.fill && tr.fill !== "none") update["fillcolor"] = hexAlpha(c, "33");
            }
            if (mode.indexOf("markers") >= 0) {
                update["marker.color"] = c;
            }
        }

        if (Object.keys(update).length > 0) Plotly.restyle(divId, update, [i]);
    });

    // Update spec/limit line shapes in layout (vlines/hlines drawn via shapes)
    var shapes = (gd.layout || {}).shapes || [];
    if (shapes.length > 0) {
        Plotly.relayout(divId, {shapes: shapes.map(function(s) {
            var col = (s.line && s.line.dash && s.line.dash !== "solid") ? t.spec : (s.line ? s.line.color : t.spec);
            // Keep dashed reference/limit lines themed; leave solid mean/median lines green-ish
            var newCol = (s.line && s.line.dash && s.line.dash !== "solid") ? t.spec : col;
            return Object.assign({}, s, {line: Object.assign({}, s.line, {color: newCol})});
        })});
    }

    var p = document.getElementById("thm_" + divId);
    if (p) p.style.display = "none";
}

function applyThemeGroup(ids, themeName) {
    ids.forEach(function(id) { applyTheme(id, themeName); });
    document.querySelectorAll("[id^=\'thm_\']").forEach(function(el){ el.style.display="none"; });
}
</script>
</body>
</html>'
                    # Guard against NULL / zero-length values reaching gsub() below —
                    # optional fields left blank in the dialog (Prepared By, Reviewed
                    # By, Company, Logo …) come through as NULL, and gsub(pattern,
                    # NULL, x) fails with "invalid 'replacement' argument". Coerce
                    # every replacement to a length-1 character string first.
                    .nz <- function(x) if (is.null(x) || length(x) == 0 || (length(x)==1 && is.na(x))) "" else as.character(x)[1]
                    .repl <- list(
                        "{{LOGO}}"    = .nz(logo_html_str),
                        "{{COMPANY}}" = .nz(company),
                        "{{GENTS}}"   = .nz(gen_ts),
                        "{{SPECOR}}"  = if (nchar(spec_ts) > 0) spec_ts else "No spec limits",
                        "{{SIGMA}}"   = .nz(sigma),
                        "{{SPECTS}}"  = .nz(spec_ts),
                        "{{VARTS}}"   = .nz(var_ts),
                        "{{PREPBY}}"  = .nz(preparedby),
                        "{{REVBY}}"   = .nz(reviewedby),
                        "{{TBLROWS}}" = .nz(tbl_rows),
                        "{{CHARTS}}"  = .nz(charts_html)
                    )
                    html_body <- html_tpl
                    for (.k in names(.repl)) html_body <- gsub(.k, .repl[[.k]], html_body, fixed=TRUE)


                    # Resolve a save directory: try the active dataset's file location,
                    # falling back to the user's home directory if it can't be determined.
                    .save_dir <- tryCatch({
                        .dsfile <- NULL
                        if (exists("spssdata.GetDataSetFile")) {
                            .dsfile <- tryCatch(spssdata.GetDataSetFile(spssdata.GetDataSetList()[1]),
                                                 error = function(e) NULL)
                        }
                        if (is.null(.dsfile) || !nzchar(.dsfile)) {
                            .dsfile <- tryCatch(spssdata.GetDataSetFileNames()[1],
                                                 error = function(e) NULL)
                        }
                        if (!is.null(.dsfile) && nzchar(.dsfile) && dir.exists(dirname(.dsfile))) {
                            dirname(.dsfile)
                        } else path.expand("~")
                    }, error = function(e) path.expand("~"))

                    # Unique file name per run so repeated saves don't overwrite each other
                    .stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
                    html_path <- file.path(.save_dir,
                                           paste0("process_capability_report_", .stamp, ".html"))
                    writeLines(html_body, con=html_path, useBytes=FALSE)

                    # Try to open the report automatically in the default browser.
                    # browseURL() can fail *silently* (it warns rather than errors, and
                    # may simply do nothing inside SPSS's embedded R session), so fall
                    # back to OS-native "open" commands and report what happened.
                    .opened <- FALSE
                    .try_open <- function(expr) {
                        ok <- tryCatch({ suppressWarnings(expr); TRUE },
                                       error = function(e) FALSE,
                                       warning = function(w) FALSE)
                        ok
                    }
                    .sysname <- tryCatch(Sys.info()[["sysname"]], error = function(e) "")
                    if (!.opened && identical(.sysname, "Darwin")) {
                        .opened <- .try_open(
                            { st <- system2("open", shQuote(html_path)); st == 0 })
                    }
                    if (!.opened && identical(.sysname, "Windows")) {
                        .opened <- .try_open({ shell.exec(html_path); TRUE })
                    }
                    if (!.opened && identical(.sysname, "Linux")) {
                        .opened <- .try_open(
                            { st <- system2("xdg-open", shQuote(html_path)); st == 0 })
                    }
                    if (!.opened) {
                        .opened <- .try_open(
                            { utils::browseURL(paste0("file://", html_path)); TRUE })
                    }
                    if (.opened) {
                        message("HTML report saved and opened: ", html_path)
                    } else {
                        message("HTML report saved to: ", html_path,
                                "  (could not open it automatically — please open it manually)")
                    }

                    spsspivottable.Display(
                        data.frame(
                            Item  = c("HTML Report", "Charts", "Interactivity"),
                            Value = c(html_path,
                                      paste(length(plt_divs), "plotly charts"),
                                      "Zoom, pan, hover, download PNG"),
                            stringsAsFactors=FALSE),
                        title="HTML Report Generated",
                        caption="Interactive plotly charts — zoom, pan, hover, download PNG")
                    }, warning = .muffle_marker_warning, message = .muffle_marker_message)
                }
            }, error=function(e) {
                warns$warn(gtxtf("HTML report error: %s", e$message))
                spsspivottable.Display(
                    data.frame(Error=e$message, stringsAsFactors=FALSE),
                    title="HTML Report Error", templateName="HTMLERR")
            })

        } # end create_charts

        # ── Close procedure & audit ───────────────────────────────────────────
        spsspkg.EndProcedure()

        # ── Generate Data: also persist the synthetic sample as a real,
        #    independently usable SPSS dataset — not just an in-memory vector
        #    fed into this one analysis run. Dataset/dictionary operations
        #    cannot run while a procedure's output containers are open, so
        #    this runs strictly AFTER EndProcedure() (mirrors the gendataset()
        #    pattern used by other R-based extensions such as
        #    STATS_OPTIMAL_DESIGNMC, which also calls its dataset-writer only
        #    once spsspkg.EndProcedure() has returned).
        if (generate_data) {
            tryCatch({
                existing_ds <- tryCatch(spssdata.GetDataSetList(), error = function(e) character(0))
                base_name   <- "GeneratedData"
                new_name    <- base_name
                suffix      <- 1L
                while (tolower(new_name) %in% tolower(existing_ds)) {
                    suffix   <- suffix + 1L
                    new_name <- paste0(base_name, suffix)
                }

                gen_df   <- data.frame(GeneratedValue = synth, stringsAsFactors = FALSE)
                gen_dict <- spssdictionary.CreateSPSSDictionary(
                    c("GeneratedValue", gtxt("Synthetic process measurement"), 0, "F8.4", "scale"))
                spssdictionary.SetDictionaryToSPSS(new_name, gen_dict)
                spssdata.SetDataToSPSS(new_name, gen_df)
                spssdictionary.EndDataStep()

                # Restore the dataset that was active before GENERATE ran, so
                # the user's working dataset isn't silently switched out from
                # under them — the new dataset remains available in the Open
                # Dataset list / via DATASET ACTIVATE for separate use.
                if (!is.null(dataset_name) && nzchar(dataset_name) &&
                    !identical(dataset_name, "Unknown") && !identical(dataset_name, "*"))
                    tryCatch(spsspkg.Submit(sprintf("DATASET ACTIVATE %s.", dataset_name)),
                             error = function(e) NULL)

                StartProcedure(procname, omsid)
                spsspivottable.Display(
                    data.frame(
                        Parameter = c(gtxt("New dataset name"), gtxt("Cases"), gtxt("Variable")),
                        Value     = c(new_name, sample_size, "GeneratedValue"),
                        stringsAsFactors = FALSE),
                    title   = gtxt("Generated Data Saved as New Dataset"),
                    caption = gtxtf(paste(
                        "The simulated sample (n = %d, mean = %s, SD = %s) has been written",
                        "to a new, independent SPSS dataset named '%s' — open it from the",
                        "Window menu / Open Dataset list, or activate it with",
                        "DATASET ACTIVATE %s. — for use outside this procedure."),
                        sample_size, format(gen_mean_val), format(gen_std_val),
                        new_name, new_name))
                spsspkg.EndProcedure()
            }, error = function(e) {
                tryCatch(spsspkg.EndProcedure(), error = function(x) NULL)
                warns$warn(gtxtf(paste(
                    "Generated sample data could not be saved as a new SPSS dataset (%s).",
                    "The simulated data was still used for the analysis above."),
                    conditionMessage(e)), dostop = FALSE)
            })
        }

        if (do_exportlogs) log_audit_event("ANALYSIS_COMPLETE", sprintf(
            "n_vars=%d; n=%d; cp=%s; cpk=%s; ppm=%s",
            length(results), if(length(results)>0) results[[1]]$n else 0,
            if(length(results)>0 && !is.na(results[[1]]$cp))  formatC(results[[1]]$cp,4,format="f")  else "NA",
            if(length(results)>0 && !is.na(results[[1]]$cpk)) formatC(results[[1]]$cpk,4,format="f") else "NA",
            if(length(results)>0 && !is.na(results[[1]]$ppm_total))
                formatC(results[[1]]$ppm_total,2,format="f") else "NA"
        ))

    }, error = function(e) {
        tryCatch(spsspkg.EndProcedure(), error = function(x) NULL)
        if (do_exportlogs) log_audit_event("ANALYSIS_ERROR", sprintf("error=%s", conditionMessage(e)))
        warns$warn(gtxtf("Analysis failed: %s", conditionMessage(e)), dostop = FALSE)
    })

    warns$display(inproc = FALSE)
}


# ══════════════════════════════════════════════════════════════════════════════
# SECTION 6 — SPSS COMMAND PARSER
# ══════════════════════════════════════════════════════════════════════════════

Run <- function(args) {
    # Force C numeric locale so plotly/JSON always uses "." not "," as decimal
    # (critical for European/Latin-American locales that use comma as decimal)
    old_lc_num <- tryCatch(Sys.getlocale("LC_NUMERIC"), error=function(e) "C")
    Sys.setlocale("LC_NUMERIC", "C")
    on.exit(tryCatch(Sys.setlocale("LC_NUMERIC", old_lc_num), error=function(e) NULL), add=TRUE)

    cmdname <- args[[1]]
    args    <- args[[2]]

    oobj <- spsspkg.Syntax(templ = list(
        # Template names UPPERCASE — must match command XML <Parameter Name="..."> exactly
        spsspkg.Template("VARIABLE",      subc="", ktype="varname", var="variable", islist=TRUE),
        spsspkg.Template("GROUPVAR",      subc="", ktype="varname", var="groupvar"),
        spsspkg.Template("LSL",           subc="CRITERIA", ktype="float",   var="lsl"),
        spsspkg.Template("USL",           subc="CRITERIA", ktype="float",   var="usl"),
        spsspkg.Template("TARGET",        subc="CRITERIA", ktype="float",   var="target"),
        spsspkg.Template("SIGMA",         subc="CRITERIA", ktype="float",   var="sigma"),
        spsspkg.Template("CONFIDENCE",    subc="CRITERIA", ktype="float",   var="confidence"),
        spsspkg.Template("SIGMAMETHOD",   subc="CRITERIA", ktype="str",     var="sigma_method"),
        spsspkg.Template("NORMALITY",     subc="STATISTICS", ktype="str",     var="normality"),
        spsspkg.Template("PPM",           subc="STATISTICS", ktype="str",     var="ppm"),
        spsspkg.Template("CPM",           subc="STATISTICS", ktype="str",     var="cpm"),
        spsspkg.Template("ZBENCH",        subc="STATISTICS", ktype="str",     var="zbench"),
        spsspkg.Template("DPMO",          subc="STATISTICS", ktype="str",     var="dpmo"),
        spsspkg.Template("YIELD",         subc="STATISTICS", ktype="str",     var="yield"),
        spsspkg.Template("SKEWNESS",      subc="STATISTICS", ktype="str",     var="skewness"),
        spsspkg.Template("PERCENTILES",   subc="STATISTICS", ktype="str",     var="percentiles"),
        spsspkg.Template("CONTROLIMR",    subc="STATISTICS", ktype="str",     var="controlimr"),
        spsspkg.Template("OUTLIERS",      subc="STATISTICS", ktype="str",     var="outliers"),
        spsspkg.Template("CIMEAN",        subc="STATISTICS", ktype="str",     var="cimean"),
        spsspkg.Template("CISTD",         subc="STATISTICS", ktype="str",     var="cistd"),
        spsspkg.Template("CICPK",         subc="STATISTICS", ktype="str",     var="cicpk"),
        spsspkg.Template("BOXCOX",        subc="OPTIONS", ktype="str",     var="boxcox"),
        spsspkg.Template("BENCHMARK",     subc="OPTIONS", ktype="str",     var="benchmark"),
        spsspkg.Template("REPORTCARD",    subc="OPTIONS", ktype="str",     var="reportcard"),
        spsspkg.Template("GENERATE",      subc="GENERATE", ktype="str",     var="generate"),
        spsspkg.Template("SAMPLESIZE",    subc="GENERATE", ktype="int",     var="samplesize",
                         vallist=list(10)),
        spsspkg.Template("MEAN",          subc="GENERATE", ktype="float",   var="mean"),
        spsspkg.Template("STDEV",         subc="GENERATE", ktype="float",   var="stdev",
                         vallist=list(0.0001)),
        spsspkg.Template("CHARTS",        subc="CHARTS", ktype="str",     var="charts"),
        spsspkg.Template("HISTOGRAM",     subc="CHARTS", ktype="str",     var="histogram"),
        spsspkg.Template("NORMALPROB",    subc="CHARTS", ktype="str",     var="normalprob"),
        spsspkg.Template("CAPABILITY",    subc="CHARTS", ktype="str",     var="capability"),
        spsspkg.Template("RUN",           subc="CHARTS", ktype="str",     var="run"),
        spsspkg.Template("BOXPLOT",       subc="CHARTS", ktype="str",     var="boxplot"),
        spsspkg.Template("SUMMARY",       subc="CHARTS", ktype="str",     var="summary"),
        spsspkg.Template("IMR",           subc="CHARTS", ktype="str",     var="imr"),
        spsspkg.Template("VIOLIN",        subc="CHARTS", ktype="str",     var="violin"),
        spsspkg.Template("KDE",           subc="CHARTS", ktype="str",     var="kde"),
        spsspkg.Template("SIXPACK",       subc="CHARTS", ktype="str",     var="sixpack"),
        spsspkg.Template("PREPAREDBY",    subc="REPORT", ktype="literal", var="preparedby"),
        spsspkg.Template("REVIEWEDBY",    subc="REPORT", ktype="literal", var="reviewedby"),
        spsspkg.Template("COMPANY",       subc="REPORT", ktype="literal", var="company"),
        spsspkg.Template("LOGOPATH",      subc="REPORT", ktype="literal", var="logopath"),
        spsspkg.Template("EXPORTLOGS",    subc="REPORT",    ktype="str",     var="exportlogs"),
        spsspkg.Template("DATADIST",    subc="DATADIST",  ktype="str",     var="datadist"),
        spsspkg.Template("DISTALPHA",   subc="DATADIST",  ktype="float",   var="dist_alpha"),
        spsspkg.Template("DIST3P",      subc="DATADIST",  ktype="str",     var="dist_include3p"),
        spsspkg.Template("DISTTRANSFORM", subc="DATADIST", ktype="str",    var="dist_transform")
    ))

    if ("HELP" %in% attr(args, "names")) {
        invisible(NULL)
    } else if (inherits(oobj, "spssError")) {
        print(oobj)
    } else {
        spsspkg.processcmd(oobj, args, "processcapability")
    }
}

# ══════════════════════════════════════════════════════════════════════════════
# END STATS_PROCESS_CAPABILITY.R  v3.0.0
# ══════════════════════════════════════════════════════════════════════════════
