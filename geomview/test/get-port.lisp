(in-package :cl-user)
(defvar sock (sb-sys::socket :inet :stream :tcp))
(defvar addr (sb-sys::socket-address sock)) 
(defvar (host (sb-sys::inet-ntoa (sb-sys::getsockname sock))) (port 0))
(sb-sys::bind sock (list 0 port))
(sb-sys::listen sock 1)
(format t "~a~%" (caddr (sb-sys::getsockname sock)))
(sb-sys::close-socket sock)


