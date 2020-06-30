(in-package :bulk)

(defun load-plist (plist instance)
  (let ((slotmap (slot-name-symbol-map instance)))
    (loop for i from 0 below (/ (length plist) 2) do 
          (let ((key (nth (* 2 i) plist))
                (val (nth (+ 1 (* 2 i)) plist)))
            (setf (slot-value instance (gethash (symbol-name key) slotmap)) val))))
  instance)

(defun slot-name-symbol-map (instance)
  (let ((amap (make-hash-table :test #'equal)))
    (mapcar (lambda (slot)
              (setf (gethash (symbol-name (closer-mop:slot-definition-name slot)) amap) 
                    (closer-mop:slot-definition-name slot)))
          (closer-mop:class-direct-slots (class-of instance)))
    amap))

(in-package :cl-user)

