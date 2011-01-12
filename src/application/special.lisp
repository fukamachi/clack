#|
  This file is a part of Slinky package.
  URL: http://github.com/fukamachi/slinky
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Slinky is freely distributable under the LLGPL License.
|#

#|
  Special variables for Slinky Application package.

  Author: Eitarow Fukamachi (fukamachi_e@ariel-networks.com)
|#

(in-package :slinky.application)

(defvar *applications* nil
  "Collections of Slinky applications have defined.")

(defvar *default-view-dir* "view"
  "Pathname of 'view' directory. It is for putting template files into.
Default is 'view'.")

(defvar *default-action-dir* "action"
  "Pathname of 'action' directory. It is for putting controller files into.
Default is 'action'.")

(defvar *default-model-dir* "model"
  "Pathname of 'model' directory. It is for putting class definition files into.
Default is 'model'.")
