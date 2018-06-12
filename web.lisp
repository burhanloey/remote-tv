(ql:quickload :hunchentoot)
(ql:quickload :spinneret)


;; Template

(defun index-page ()
  (spinneret:with-html-string
    (:doctype)
    (:html
     (:head
      (:title "Remote TV")
      (:link :rel "stylesheet" :href "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css")
      (:link :rel "stylesheet" :href "https://fonts.googleapis.com/css?family=Lato|Oswald")
      (:link :rel "stylesheet" :href "/css/style.css"))
     (:body
      (:section.col-md-4.col-md-offset-4
       (:h1.text-center "Remote TV")
       (:div.panel.panel-default
        (:div.panel-body
         (:form :method "post" :enctype "multipart/form-data"
                :action "javascript:submit()"
                (:div.form-group
                 (:label :for "video" "Video")
                 (:input#video :type "file" :name "video"))
                (:div.form-group
                 (:label :for "subtitle" "Subtitle")
                 (:input#subtitle :type "file" :name "subtitle"))
                (:input#upload.btn.btn-primary.btn-block
                 :type "submit" :value "Upload"))
         (:br)
         (:div.progress
          (:div.progress-bar.progress-bar-info
           :role "progressbar" :aria-valuenow "0" :aria-valuemin "0"
           :aria-valuemax "100" :style "width: 0%"
           (:span "0%"))))))
      (:script :src "/js/upload.js")))))


;; Util functions

(defun save-uploaded-file (post-param)
  "Move uploaded binary to a file using filename used during upload. If it is
not moved/copied it will be gone after request. Returns file path for the new
file."
  (when (and (listp post-param)
             (= (length post-param) 3))
    (destructuring-bind (path file-name content-type) post-param
      (declare (ignore content-type))
      (let ((new-path (merge-pathnames file-name path)))
        (uiop:run-program (format nil "mv ~a ~a" path new-path))
        new-path))))

(defun mpv (video-path subtitle-path)
  "Launch mpv to play the video with subtitle (if supplied). Had to run on
separate thread instead of using launch-program since the sbcl version for armhf
doesn't have launch-program function."
  (let ((cmd (if subtitle-path
                 (format nil "mpv ~a --sub-file ~a" video-path subtitle-path)
                 (format nil "mpv ~a" video-path))))
    (bordeaux-threads:make-thread
     (lambda () (uiop:run-program cmd)))))

(defun omxplayer (video-path subtitle-path)
  "Launch omxplayer to play the video with subtitle (if supplied). Had to run on
separate thread instead of using launch-program since the sbcl version for armhf
doesn't have launch-program function."
  (let ((cmd (if subtitle-path
                 (format
                  nil "omxplayer -o hdmi ~a --subtitles ~a --align center --blank"
                  video-path subtitle-path)
                 (format nil "omxplayer -o hdmi ~a --blank" video-path))))
    (bordeaux-threads:make-thread
     (lambda () (uiop:run-program cmd)))))


;; Handlers

(hunchentoot:define-easy-handler (index :uri "/") ()
  (case (hunchentoot:request-method*)
    (:get
     (index-page))))

(hunchentoot:define-easy-handler (play :uri "/play") ()
  (case (hunchentoot:request-method*)
    (:post
     (let ((video-path    (save-uploaded-file
                           (hunchentoot:post-parameter "video")))
           (subtitle-path (save-uploaded-file
                           (hunchentoot:post-parameter "subtitle"))))
       (if video-path
           (progn
             (omxplayer video-path subtitle-path)
             (format nil "Siap upload. Video dah mula dimainkan. Boleh tutup website ni."))
           (format nil "Takde video pun."))))))


;; Static files handlers

(push
 (hunchentoot:create-folder-dispatcher-and-handler
  "/js/" (merge-pathnames #p"remote-tv/js/" (user-homedir-pathname)))
 hunchentoot:*dispatch-table*)

(push
 (hunchentoot:create-folder-dispatcher-and-handler
  "/css/" (merge-pathnames #p"remote-tv/css/" (user-homedir-pathname)))
 hunchentoot:*dispatch-table*)


;; Server

(defparameter *serv*
  (let* ((port-string (uiop:getenv "PORT"))
         (port        (when port-string (parse-integer port-string))))
    (make-instance 'hunchentoot:easy-acceptor :port (or port 3000))))

(hunchentoot:start *serv*)
;; (hunchentoot:stop *serv*)
