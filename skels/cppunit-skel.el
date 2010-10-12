;;; cppunit-skel.el

;; Copyright (C) 2004, 2005 Jose Antonio Ortega Ruiz

;; Author: Jose A Ortega Ruiz <jao@gnu.org>
;; Keywords: tools

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Skeletons creating cppunit classes.

;;; Code:

(require 'common-skel)

(define-skeleton jao-cppunit-main
  "Insert CPPUNIT main function"
  nil
  "#include <cppunit/extensions/TestFactoryRegistry.h>" > \n
  "#include <cppunit/ui/text/TestRunner.h>" > \n \n
  "int" > \n
  "main(int argc, char* argv[])" > \n
  "{" > \n
  "CppUnit::TextUi::TestRunner runner;" > \n
  "CppUnit::TestFactoryRegistry& registry =" > \n
  "CppUnit::TestFactoryRegistry::getRegistry();" > \n \n
  "runner.addTest(registry.makeTest());" > \n \n
  "return !runner.run(\"\", false);" > \n
  "}" > \n)

(define-skeleton jao-cppunit-class
  "Create a CPPUNIT class definition preamble"
  nil
  >
  "CPPUNIT_TEST_SUITE(" (jao-basename) ");"
  > \n
  "CPPUNIT_TEST(test);"
  > \n
  "CPPUNIT_TEST_SUITE_END();"
  > \n \n
  "private:"
  > \n \n
  "void test();"
  > \n \n
  "private:"
  > \n \n
  "void set_up();"
  > \n
  "void tear_down();"
  > \n)

(define-skeleton jao-cppunit-classdef
  "Create a CPPUNIT class implementation preamble"
  nil
  >
  "CPPUNIT_TEST_SUITE_REGISTRATION(" (jao-basename) ");"
  > \n \n
  "void"
  > \n
  (jao-basename) "::set_up()"
  > \n
  "{"
  > \n
  "}"
  > \n \n
  "void"
  > \n
  (jao-basename) "::tear_down()"
  > \n
  "{"
  > \n
  "}"
  > \n)

(provide 'cppunit-skel)

