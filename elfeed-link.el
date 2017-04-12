;;; elfeed-link.el --- misc functions for elfeed -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;;; Commentary:

;; Code for integration with org-mode.

;; To use, add (require 'elfeed-link) somewhere in your configuration.

;;; Code:

(require 'elfeed-db)
(require 'elfeed-show)
(require 'elfeed-search)

(require 'org)

(defun elfeed-link-store-link ()
  "Store a link to an elfeed search or entry buffer.

When storing a link to an entry, automatically extract all the
entry metadata.  These can be used in the capture templates as
%:elfeed-entry-<prop>.  See `elfeed-entry--create' for the list
of available props."
  (cond ((derived-mode-p 'elfeed-search-mode)
         (org-store-link-props
          :type "elfeed"
          :link (format "elfeed:%s" elfeed-search-filter)
          :description elfeed-search-filter))
        ((derived-mode-p 'elfeed-show-mode)
         (apply
          'org-store-link-props
          :type "elfeed"
          :link (format "elfeed:%s#%s"
                        (car (elfeed-entry-id elfeed-show-entry))
                        (cdr (elfeed-entry-id elfeed-show-entry)))
          :description (elfeed-entry-title elfeed-show-entry)
           (cl-loop for (prop) in (cdr (plist-get
                                        (symbol-plist 'elfeed-entry)
                                        'cl-struct-slots))
                    nconc (list
                           (intern (concat ":elfeed-entry-" (symbol-name prop)))
                           (funcall
                            (intern (concat "elfeed-entry-" (symbol-name prop)))
                            elfeed-show-entry)))))))

(defun elfeed-link-open (filter-or-id)
  "Jump to an elfeed entry or search.

Depending on what FILTER-OR-ID looks like, we jump to either
search buffer or show a concrete entry."
  (if (string-match "\\([^#]+\\)#\\(.+\\)" filter-or-id)
      (elfeed-show-entry (elfeed-db-get-entry
                          (cons (match-string 1 filter-or-id)
                                (match-string 2 filter-or-id))))
    (switch-to-buffer (elfeed-search-buffer))
    (unless (eq major-mode 'elfeed-search-mode)
      (elfeed-search-mode))
    (elfeed-search-set-filter filter-or-id)))

;;;###autoload
(eval-after-load 'org
  '(if (version< (org-version) "9.0")
       (progn
         (org-add-link-type "elfeed" #'elfeed-link-open)
         (add-hook 'org-store-link-functions #'elfeed-link-store-link))
     (with-no-warnings
       (org-link-set-parameters
        "elfeed"
        :follow #'elfeed-link-open
        :store #'elfeed-link-store-link))))

(provide 'elfeed-link)

;;; elfeed-link.el ends here
