    (org-set-generic-type "wiki"
     '(:file-suffix        	    ".txt"
     :key-binding                   ?b

     :header-prefix            	    ""
     :header-suffix            	    ""

     :title-format             	    "= %s =\n"

     :date-export        	    nil

     :toc-export                    nil

     :body-header-section-numbers   nil
     :body-section-prefix           "\n"

     :body-section-header-prefix    ("== " "* " "** "
				     "*** " "**** " "***** ")
     :body-section-header-suffix    (" ==\n" " \n" " \n" 
				     " \n" " \n" " \n")

     :body-line-export-preformated  t          ;; yes/no/maybe???
     :body-line-format              "body-line-format %s\n"
     ;; :body-line-wrap                75

     :body-line-fixed-format       "body-line-fixed %s\n"

     :body-list-format              "** body-list-format %s\n"
     :body-number-list-format       "# %s\n"

     :body-bullet-list-prefix       ("** " "*** " "**** " "***** " "****** ")
     ))
