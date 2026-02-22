;;; Sanctify-PHP Hardened WordPress Container
;;; SPDX-License-Identifier: MPL-2.0-or-later
;;;
;;; This Guix manifest provides infrastructure-level hardening that
;;; complements the code-level analysis from sanctify-php.
;;;
;;; Sphere of Influence Extension:
;;; - sanctify-php: transforms code, adds escaping, type hints
;;; - This container: enforces runtime constraints code cannot bypass

(use-modules (guix packages)
             (guix gexp)
             (gnu packages php)
             (gnu packages web)
             (gnu services web)
             (gnu services)
             (gnu system)
             (gnu system file-systems))

;;; === PHP Configuration ===
;;; These settings ENFORCE what sanctify-php RECOMMENDS

(define %hardened-php-ini
  "
; === SECURITY: Disable dangerous functions ===
; sanctify-php warns about these; we BLOCK them
disable_functions = exec,passthru,shell_exec,system,proc_open,popen,curl_exec,curl_multi_exec,parse_ini_file,show_source,eval,create_function,assert

; === SECURITY: Limit file operations ===
open_basedir = /var/www/wordpress:/tmp:/var/lib/php/sessions
upload_tmp_dir = /tmp
sys_temp_dir = /tmp

; === SECURITY: Session hardening ===
session.cookie_httponly = 1
session.cookie_secure = 1
session.cookie_samesite = Strict
session.use_strict_mode = 1

; === TYPE SAFETY: Matches sanctify-php strict_types ===
; Note: This doesn't add declare(), but makes PHP stricter overall
zend.exception_ignore_args = On
zend.exception_string_param_max_len = 0

; === ERROR HANDLING: Production mode ===
display_errors = Off
log_errors = On
error_log = /var/log/php/error.log
error_reporting = E_ALL & ~E_DEPRECATED & ~E_STRICT

; === RESOURCE LIMITS ===
max_execution_time = 30
max_input_time = 30
memory_limit = 128M
post_max_size = 32M
upload_max_filesize = 16M
max_file_uploads = 10

; === OPCACHE: Performance + security ===
opcache.enable = 1
opcache.validate_timestamps = 0  ; Don't check for file changes
opcache.file_cache_only = 0
opcache.huge_code_pages = 1
")

;;; === Web Server Configuration ===
;;; Security headers that PHP code cannot set reliably

(define %hardened-nginx-conf
  "
# === SECURITY HEADERS ===
# These protect against attacks sanctify-php cannot prevent

add_header X-Frame-Options \"SAMEORIGIN\" always;
add_header X-Content-Type-Options \"nosniff\" always;
add_header X-XSS-Protection \"1; mode=block\" always;
add_header Referrer-Policy \"strict-origin-when-cross-origin\" always;
add_header Permissions-Policy \"geolocation=(), microphone=(), camera=()\" always;

# Content Security Policy - WordPress compatible
add_header Content-Security-Policy \"default-src 'self'; script-src 'self' 'unsafe-inline' 'unsafe-eval'; style-src 'self' 'unsafe-inline'; img-src 'self' data: https:; font-src 'self' data:; connect-src 'self'; frame-ancestors 'self';\" always;

# === BLOCK SENSITIVE FILES ===
# Even if sanctify-php misses something
location ~ /\\. { deny all; }
location ~ ^/(wp-config\\.php|readme\\.html|license\\.txt) { deny all; }
location ~ /wp-content/uploads/.*\\.php$ { deny all; }
location ~ /wp-includes/.*\\.php$ { deny all; }

# === RATE LIMITING ===
# Protect wp-login.php from brute force
location = /wp-login.php {
    limit_req zone=login burst=5 nodelay;
    include fastcgi_params;
    fastcgi_pass php;
}

# === BLOCK XMLRPC ===
# Common attack vector
location = /xmlrpc.php { deny all; }
")

;;; === Container Definition ===

(define %wordpress-hardened-container
  (operating-system
    (host-name "wp-hardened")
    (timezone "UTC")

    (file-systems
     (cons*
      ;; Read-only WordPress code
      (file-system
       (device "/app")
       (mount-point "/var/www/wordpress")
       (type "none")
       (flags '(bind-mount read-only)))
      ;; Writable only for uploads
      (file-system
       (device "/data/uploads")
       (mount-point "/var/www/wordpress/wp-content/uploads")
       (type "none")
       (flags '(bind-mount)))
      %base-file-systems))

    (services
     (list
      ;; PHP-FPM with hardened config
      (service php-fpm-service-type
               (php-fpm-configuration
                (php-ini-file %hardened-php-ini)
                (workers 4)
                (process-manager 'static)))

      ;; Nginx with security headers
      (service nginx-service-type
               (nginx-configuration
                (extra-content %hardened-nginx-conf)))))))

;;; === Sanctify Integration Points ===
;;;
;;; The sanctify-php tool can output:
;;; 1. php.ini recommendations -> merged into %hardened-php-ini
;;; 2. nginx security rules -> merged into %hardened-nginx-conf
;;; 3. file permission map -> used for read-only mounts
;;;
;;; Example workflow:
;;; $ sanctify analyze ./wp-content/plugins/my-plugin/
;;; $ sanctify export --format=guix-php-ini >> container-overrides.scm
;;; $ guix system container wordpress.scm -L ./container-overrides.scm

;;; === Sidecar Services ===
;;;
;;; For functionality that shouldn't be in PHP at all:

(define %safer-sidecars
  '(;; Email sending - use a Rust/Go service instead of PHP mail()
    (service "smtp-relay"
             (image "mailhog/mailhog")
             (ports '("1025:1025")))

    ;; Image processing - ImageMagick vulnerabilities are common
    (service "image-processor"
             (image "custom/libvips-service")
             (ports '("8080:8080")))

    ;; PDF generation - avoid PHP libraries
    (service "pdf-generator"
             (image "custom/weasyprint-service")
             (ports '("8081:8081")))))

;;; === Audit Logging ===
;;;
;;; Log all PHP function calls that sanctify-php flagged as dangerous
;;; Even if the code slips through, we have audit trail

(define %audit-rules
  "
# Log all exec-family calls (shouldn't happen with disable_functions)
-a always,exit -F arch=b64 -S execve -k php_exec

# Log file access outside open_basedir (shouldn't happen)
-w /etc/passwd -p r -k php_file_access
-w /etc/shadow -p r -k php_file_access

# Log network connections from PHP process
-a always,exit -F arch=b64 -S connect -F uid=www-data -k php_network
")

%wordpress-hardened-container
