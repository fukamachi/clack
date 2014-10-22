#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitaro Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

;;; Copyright (c) 2004-2010, Dr. Edmund Weitz. All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :cl-user)
(defpackage clack.util.hunchentoot
  (:use :cl
        :flexi-streams))
(in-package :clack.util.hunchentoot)

(cl-syntax:use-syntax :annot)

(defparameter *mime-type-list* '(("application/andrew-inset" "ez")
                                 ("application/cu-seeme" "cu")
                                 ("application/dsptype" "tsp")
                                 ("application/futuresplash" "spl")
                                 ("application/hta" "hta")
                                 ("application/java-archive" "jar")
                                 ("application/java-serialized-object" "ser")
                                 ("application/java-vm" "class")
                                 ("application/mac-binhex40" "hqx")
                                 ("application/mac-compactpro" "cpt")
                                 ("application/mathematica" "nb")
                                 ("application/msaccess" "mdb")
                                 ("application/msword" "doc" "dot")
                                 ("application/octet-stream" "bin")
                                 ("application/oda" "oda")
                                 ("application/ogg" "ogg")
                                 ("application/pdf" "pdf")
                                 ("application/pgp-keys" "key")
                                 ("application/pgp-signature" "pgp")
                                 ("application/pics-rules" "prf")
                                 ("application/postscript" "ps" "ai" "eps")
                                 ("application/rar" "rar")
                                 ("application/rdf+xml" "rdf")
                                 ("application/rss+xml" "rss")
                                 ("application/smil" "smi" "smil")
                                 ("application/wordperfect" "wpd")
                                 ("application/wordperfect5.1" "wp5")
                                 ("application/xhtml+xml" "xhtml" "xht")
                                 ("application/xml" "fo" "xml" "xsl")
                                 ("application/zip" "zip")
                                 ("application/vnd.cinderella" "cdy")
                                 ("application/vnd.mozilla.xul+xml" "xul")
                                 ("application/vnd.ms-excel" "xls" "xlb" "xlt")
                                 ("application/vnd.ms-pki.seccat" "cat")
                                 ("application/vnd.ms-pki.stl" "stl")
                                 ("application/vnd.ms-powerpoint" "ppt" "pps")
                                 ("application/vnd.oasis.opendocument.chart" "odc")
                                 ("application/vnd.oasis.opendocument.database" "odb")
                                 ("application/vnd.oasis.opendocument.formula" "odf")
                                 ("application/vnd.oasis.opendocument.graphics" "odg")
                                 ("application/vnd.oasis.opendocument.graphics-template" "otg")
                                 ("application/vnd.oasis.opendocument.image" "odi")
                                 ("application/vnd.oasis.opendocument.presentation" "odp")
                                 ("application/vnd.oasis.opendocument.presentation-template" "otp")
                                 ("application/vnd.oasis.opendocument.spreadsheet" "ods")
                                 ("application/vnd.oasis.opendocument.spreadsheet-template" "ots")
                                 ("application/vnd.oasis.opendocument.text" "odt")
                                 ("application/vnd.oasis.opendocument.text-master" "odm")
                                 ("application/vnd.oasis.opendocument.text-template" "ott")
                                 ("application/vnd.oasis.opendocument.text-web" "oth")
                                 ("application/vnd.rim.cod" "cod")
                                 ("application/vnd.smaf" "mmf")
                                 ("application/vnd.stardivision.calc" "sdc")
                                 ("application/vnd.stardivision.draw" "sda")
                                 ("application/vnd.stardivision.impress" "sdd" "sdp")
                                 ("application/vnd.stardivision.math" "smf")
                                 ("application/vnd.stardivision.writer" "sdw" "vor")
                                 ("application/vnd.stardivision.writer-global" "sgl")
                                 ("application/vnd.sun.xml.calc" "sxc")
                                 ("application/vnd.sun.xml.calc.template" "stc")
                                 ("application/vnd.sun.xml.draw" "sxd")
                                 ("application/vnd.sun.xml.draw.template" "std")
                                 ("application/vnd.sun.xml.impress" "sxi")
                                 ("application/vnd.sun.xml.impress.template" "sti")
                                 ("application/vnd.sun.xml.math" "sxm")
                                 ("application/vnd.sun.xml.writer" "sxw")
                                 ("application/vnd.sun.xml.writer.global" "sxg")
                                 ("application/vnd.sun.xml.writer.template" "stw")
                                 ("application/vnd.symbian.install" "sis")
                                 ("application/vnd.visio" "vsd")
                                 ("application/vnd.wap.wbxml" "wbxml")
                                 ("application/vnd.wap.wmlc" "wmlc")
                                 ("application/vnd.wap.wmlscriptc" "wmlsc")
                                 ("application/x-123" "wk")
                                 ("application/x-abiword" "abw")
                                 ("application/x-apple-diskimage" "dmg")
                                 ("application/x-bcpio" "bcpio")
                                 ("application/x-bittorrent" "torrent")
                                 ("application/x-cdf" "cdf")
                                 ("application/x-cdlink" "vcd")
                                 ("application/x-chess-pgn" "pgn")
                                 ("application/x-cpio" "cpio")
                                 ("application/x-csh" "csh")
                                 ("application/x-debian-package" "deb" "udeb")
                                 ("application/x-director" "dcr" "dir" "dxr")
                                 ("application/x-dms" "dms")
                                 ("application/x-doom" "wad")
                                 ("application/x-dvi" "dvi")
                                 ("application/x-flac" "flac")
                                 ("application/x-font" "pfa" "pfb" "gsf" "pcf")
                                 ("application/x-freemind" "mm")
                                 ("application/x-futuresplash" "spl")
                                 ("application/x-gnumeric" "gnumeric")
                                 ("application/x-go-sgf" "sgf")
                                 ("application/x-graphing-calculator" "gcf")
                                 ("application/x-gtar" "gtar" "tgz" "taz")
                                 ("application/x-hdf" "hdf")
                                 ("application/x-httpd-php" "phtml" "pht" "php")
                                 ("application/x-httpd-php-source" "phps")
                                 ("application/x-httpd-php3" "php3")
                                 ("application/x-httpd-php3-preprocessed" "php3p")
                                 ("application/x-httpd-php4" "php4")
                                 ("application/x-ica" "ica")
                                 ("application/x-internet-signup" "ins" "isp")
                                 ("application/x-iphone" "iii")
                                 ("application/x-iso9660-image" "iso")
                                 ("application/x-java-jnlp-file" "jnlp")
                                 ("application/x-javascript" "js")
                                 ("application/x-jmol" "jmz")
                                 ("application/x-kchart" "chrt")
                                 ("application/x-killustrator" "kil")
                                 ("application/x-koan" "skp" "skd" "skt" "skm")
                                 ("application/x-kpresenter" "kpr" "kpt")
                                 ("application/x-kspread" "ksp")
                                 ("application/x-kword" "kwd" "kwt")
                                 ("application/x-latex" "latex")
                                 ("application/x-lha" "lha")
                                 ("application/x-lzh" "lzh")
                                 ("application/x-lzx" "lzx")
                                 ("application/x-maker" "frm" "maker" "frame" "fm" "fb" "book" "fbdoc")
                                 ("application/x-mif" "mif")
                                 ("application/x-ms-wmd" "wmd")
                                 ("application/x-ms-wmz" "wmz")
                                 ("application/x-msdos-program" "com" "exe" "bat" "dll")
                                 ("application/x-msi" "msi")
                                 ("application/x-netcdf" "nc")
                                 ("application/x-ns-proxy-autoconfig" "pac")
                                 ("application/x-nwc" "nwc")
                                 ("application/x-object" "o")
                                 ("application/x-oz-application" "oza")
                                 ("application/x-pkcs7-certreqresp" "p7r")
                                 ("application/x-pkcs7-crl" "crl")
                                 ("application/x-python-code" "pyc" "pyo")
                                 ("application/x-quicktimeplayer" "qtl")
                                 ("application/x-redhat-package-manager" "rpm")
                                 ("application/x-sh" "sh")
                                 ("application/x-shar" "shar")
                                 ("application/x-shockwave-flash" "swf" "swfl")
                                 ("application/x-stuffit" "sit")
                                 ("application/x-sv4cpio" "sv4cpio")
                                 ("application/x-sv4crc" "sv4crc")
                                 ("application/x-tar" "tar")
                                 ("application/x-tcl" "tcl")
                                 ("application/x-tex-gf" "gf")
                                 ("application/x-tex-pk" "pk")
                                 ("application/x-texinfo" "texinfo" "texi")
                                 ("application/x-trash" "~%" "" "bak" "old" "sik")
                                 ("application/x-troff" "tt" "r" "roff")
                                 ("application/x-troff-man" "man")
                                 ("application/x-troff-me" "me")
                                 ("application/x-troff-ms" "ms")
                                 ("application/x-ustar" "ustar")
                                 ("application/x-wais-source" "src")
                                 ("application/x-wingz" "wz")
                                 ("application/x-x509-ca-cert" "crt")
                                 ("application/x-xcf" "xcf")
                                 ("application/x-xfig" "fig")
                                 ("application/x-xpinstall" "xpi")
                                 ("audio/basic" "au" "snd")
                                 ("audio/midi" "mid" "midi" "kar")
                                 ("audio/mpeg" "mpga" "mpega" "mp2" "mp3" "m4a")
                                 ("audio/mpegurl" "m3u")
                                 ("audio/prs.sid" "sid")
                                 ("audio/x-aiff" "aif" "aiff" "aifc")
                                 ("audio/x-gsm" "gsm")
                                 ("audio/x-mpegurl" "m3u")
                                 ("audio/x-ms-wma" "wma")
                                 ("audio/x-ms-wax" "wax")
                                 ("audio/x-pn-realaudio" "ra" "rm" "ram")
                                 ("audio/x-realaudio" "ra")
                                 ("audio/x-scpls" "pls")
                                 ("audio/x-sd2" "sd2")
                                 ("audio/x-wav" "wav")
                                 ("chemical/x-alchemy" "alc")
                                 ("chemical/x-cache" "cac" "cache")
                                 ("chemical/x-cache-csf" "csf")
                                 ("chemical/x-cactvs-binary" "cbin" "cascii" "ctab")
                                 ("chemical/x-cdx" "cdx")
                                 ("chemical/x-cerius" "cer")
                                 ("chemical/x-chem3d" "c3d")
                                 ("chemical/x-chemdraw" "chm")
                                 ("chemical/x-cif" "cif")
                                 ("chemical/x-cmdf" "cmdf")
                                 ("chemical/x-cml" "cml")
                                 ("chemical/x-compass" "cpa")
                                 ("chemical/x-crossfire" "bsd")
                                 ("chemical/x-csml" "csml" "csm")
                                 ("chemical/x-ctx" "ctx")
                                 ("chemical/x-cxf" "cxf" "cef")
                                 ("chemical/x-embl-dl-nucleotide" "emb" "embl")
                                 ("chemical/x-galactic-spc" "spc")
                                 ("chemical/x-gamess-input" "inp" "gam" "gamin")
                                 ("chemical/x-gaussian-checkpoint" "fch" "fchk")
                                 ("chemical/x-gaussian-cube" "cub")
                                 ("chemical/x-gaussian-input" "gau" "gjc" "gjf")
                                 ("chemical/x-gaussian-log" "gal")
                                 ("chemical/x-gcg8-sequence" "gcg")
                                 ("chemical/x-genbank" "gen")
                                 ("chemical/x-hin" "hin")
                                 ("chemical/x-isostar" "istr" "ist")
                                 ("chemical/x-jcamp-dx" "jdx" "dx")
                                 ("chemical/x-kinemage" "kin")
                                 ("chemical/x-macmolecule" "mcm")
                                 ("chemical/x-macromodel-input" "mmd" "mmod")
                                 ("chemical/x-mdl-molfile" "mol")
                                 ("chemical/x-mdl-rdfile" "rd")
                                 ("chemical/x-mdl-rxnfile" "rxn")
                                 ("chemical/x-mdl-sdfile" "sd" "sdf")
                                 ("chemical/x-mdl-tgf" "tgf")
                                 ("chemical/x-mmcif" "mcif")
                                 ("chemical/x-mol2" "mol2")
                                 ("chemical/x-molconn-Z" "b")
                                 ("chemical/x-mopac-graph" "gpt")
                                 ("chemical/x-mopac-input" "mop" "mopcrt" "mpc" "dat" "zmt")
                                 ("chemical/x-mopac-out" "moo")
                                 ("chemical/x-mopac-vib" "mvb")
                                 ("chemical/x-ncbi-asn1" "asn")
                                 ("chemical/x-ncbi-asn1-ascii" "prt" "ent")
                                 ("chemical/x-ncbi-asn1-binary" "val" "aso")
                                 ("chemical/x-ncbi-asn1-spec" "asn")
                                 ("chemical/x-pdb" "pdb" "ent")
                                 ("chemical/x-rosdal" "ros")
                                 ("chemical/x-swissprot" "sw")
                                 ("chemical/x-vamas-iso14976" "vms")
                                 ("chemical/x-vmd" "vmd")
                                 ("chemical/x-xtel" "xtel")
                                 ("chemical/x-xyz" "xyz")
                                 ("image/gif" "gif")
                                 ("image/ief" "ief")
                                 ("image/jpeg" "jpeg" "jpg" "jpe")
                                 ("image/pcx" "pcx")
                                 ("image/png" "png")
                                 ("image/svg+xml" "svg" "svgz")
                                 ("image/tiff" "tiff" "tif")
                                 ("image/vnd.djvu" "djvu" "djv")
                                 ("image/vnd.wap.wbmp" "wbmp")
                                 ("image/x-cmu-raster" "ras")
                                 ("image/x-coreldraw" "cdr")
                                 ("image/x-coreldrawpattern" "pat")
                                 ("image/x-coreldrawtemplate" "cdt")
                                 ("image/x-corelphotopaint" "cpt")
                                 ("image/x-icon" "ico")
                                 ("image/x-jg" "art")
                                 ("image/x-jng" "jng")
                                 ("image/x-ms-bmp" "bmp")
                                 ("image/x-photoshop" "psd")
                                 ("image/x-portable-anymap" "pnm")
                                 ("image/x-portable-bitmap" "pbm")
                                 ("image/x-portable-graymap" "pgm")
                                 ("image/x-portable-pixmap" "ppm")
                                 ("image/x-rgb" "rgb")
                                 ("image/x-xbitmap" "xbm")
                                 ("image/x-xpixmap" "xpm")
                                 ("image/x-xwindowdump" "xwd")
                                 ("model/iges" "igs" "iges")
                                 ("model/mesh" "msh" "mesh" "silo")
                                 ("model/vrml" "wrl" "vrml")
                                 ("text/calendar" "ics" "icz")
                                 ("text/comma-separated-values" "csv")
                                 ("text/css" "css")
                                 ("text/h323" "323")
                                 ("text/html" "html" "htm" "shtml")
                                 ("text/iuls" "uls")
                                 ("text/mathml" "mml")
                                 ("text/plain" "asc" "txt" "text" "diff" "pot")
                                 ("text/richtext" "rtx")
                                 ("text/rtf" "rtf")
                                 ("text/scriptlet" "sct" "wsc")
                                 ("text/texmacs" "tm" "ts")
                                 ("text/tab-separated-values" "tsv")
                                 ("text/vnd.sun.j2me.app-descriptor" "jad")
                                 ("text/vnd.wap.wml" "wml")
                                 ("text/vnd.wap.wmlscript" "wmls")
                                 ("text/x-bibtex" "bib")
                                 ("text/x-boo" "boo")
                                 ("text/x-c++hdr" "h++" "hpp" "hxx" "hh")
                                 ("text/x-c++src" "c++" "cpp" "cxx" "cc")
                                 ("text/x-chdr" "h")
                                 ("text/x-component" "htc")
                                 ("text/x-csh" "csh")
                                 ("text/x-csrc" "c")
                                 ("text/x-dsrc" "d")
                                 ("text/x-haskell" "hs")
                                 ("text/x-java" "java")
                                 ("text/javascript" "js")
                                 ("text/x-literate-haskell" "lhs")
                                 ("text/x-moc" "moc")
                                 ("text/x-pascal" "pp" "as")
                                 ("text/x-pcs-gcd" "gcd")
                                 ("text/x-perl" "pl" "pm")
                                 ("text/x-python" "py")
                                 ("text/x-setext" "etx")
                                 ("text/x-sh" "sh")
                                 ("text/x-tcl" "tcl" "tk")
                                 ("text/x-tex" "tex" "ltx" "sty" "cls")
                                 ("text/x-vcalendar" "vcs")
                                 ("text/x-vcard" "vcf")
                                 ("video/dl" "dl")
                                 ("video/dv" "dif" "dv")
                                 ("video/fli" "fli")
                                 ("video/gl" "gl")
                                 ("video/mpeg" "mpeg" "mpg" "mpe")
                                 ("video/mp4" "mp4")
                                 ("video/quicktime" "qt" "mov")
                                 ("video/vnd.mpegurl" "mxu")
                                 ("video/x-la-asf" "lsf" "lsx")
                                 ("video/x-mng" "mng")
                                 ("video/x-ms-asf" "asf" "asx")
                                 ("video/x-ms-wm" "wm")
                                 ("video/x-ms-wmv" "wmv")
                                 ("video/x-ms-wmx" "wmx")
                                 ("video/x-ms-wvx" "wvx")
                                 ("video/x-msvideo" "avi")
                                 ("video/x-sgi-movie" "movie")
                                 ("x-conference/x-cooltalk" "ice")
                                 ("x-world/x-vrml" "vrm" "vrml" "wrl"))
  "An alist where the cars are MIME types and the cdrs are list
of file suffixes for the corresponding type.")

(defparameter *mime-type-hash*
  (let ((hash (make-hash-table :test #'equalp)))
    (loop for (type . suffixes) in *mime-type-list* do
          (loop for suffix in suffixes do
                (setf (gethash suffix hash) type)))
    hash)
  "A hash table which maps file suffixes to MIME types.")

@export
(defun mime-type (pathspec)
  "Given a pathname designator PATHSPEC returns the MIME type
\(as a string) corresponding to the suffix of the file denoted by
PATHSPEC \(or NIL)."
  (gethash (pathname-type pathspec) *mime-type-hash*))

(defvar *default-external-format*
    (make-external-format :utf-8 :eol-style :lf)
  "The external format used to compute the REQUEST object.")

(defmacro upgrade-vector (vector new-type &key converter)
  "Returns a vector with the same length and the same elements as
VECTOR \(a variable holding a vector) but having element type
NEW-TYPE.  If CONVERTER is not NIL, it should designate a function
which will be applied to each element of VECTOR before the result is
stored in the new vector.  The resulting vector will have a fill
pointer set to its end.

The macro also uses SETQ to store the new vector in VECTOR."
  `(setq ,vector
         (loop with length = (length ,vector)
               with new-vector = (make-array length
                                             :element-type ,new-type
                                             :fill-pointer length)
               for i below length
               do (setf (aref new-vector i) ,(if converter
                                               `(funcall ,converter (aref ,vector i))
                                               `(aref ,vector i)))
               finally (return new-vector))))

@export
(defun url-decode (string &optional (external-format *default-external-format*))
  "Decodes a URL-encoded STRING which is assumed to be encoded using
the external format EXTERNAL-FORMAT."
  (when (zerop (length string))
    (return-from url-decode ""))
  (let ((vector (make-array (length string) :element-type 'octet :fill-pointer 0))
        (i 0)
        unicodep)
    (loop
      (unless (< i (length string))
        (return))
      (let ((char (aref string i)))
       (labels ((decode-hex (length)
                  (prog1
                      (parse-integer string :start i :end (+ i length) :radix 16)
                    (incf i length)))
                (push-integer (integer)
                  (vector-push integer vector))
                (peek ()
                  (aref string i))
                (advance ()
                  (setq char (peek))
                  (incf i)))
         (cond
          ((char= #\% char)
           (advance)
           (cond
            ((char= #\u (peek))
             (unless unicodep
               (setq unicodep t)
               (upgrade-vector vector '(integer 0 65535)))
             (advance)
             (push-integer (decode-hex 4)))
            (t
             (push-integer (decode-hex 2)))))
          (t
           (push-integer (char-code (case char
                                      ((#\+) #\Space)
                                      (otherwise char))))
           (advance))))))
    (cond (unicodep
           (upgrade-vector vector 'character :converter #'code-char))
          (t (octets-to-string vector :external-format external-format)))))

(doc:start)

@doc:NAME "
Clack.Util.Hunchentoot - Ported from Hunchentoot, Lisp Web server.
"

@doc:DESCRIPTION "
Hunchentoot is great product, but it is too huge as a Web server. It has also useful utility.
Though I don't like to Clack depends on Hunchentoot. So, I had decided to port needed utilities in Hunchentoot to this package.

From such details, only these function and variables, in this file, under BSD-style license, same as Hunchentoot's one.

Thanks Edi!
"

@doc:AUTHOR "
* Eitaro Fukamachi (e.arrows@gmail.com)
* Dr. Edmund Weitz [Original Writer]
"
