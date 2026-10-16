;; Operator wrapper for the classpath-backed installer.
(do
  (require 'futon3c.wm.cast-registry :reload)
  ((resolve 'futon3c.wm.cast-registry/install!)))
