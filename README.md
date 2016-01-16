# UWapiImplement-racket-
A small racket function which makes UWaterlooAPI more user friendly  
use function (search (req (course (list (subject ...))))) to get all viable course schedule according to your requirement   
(req) is the function which makes a (req) subject.A Requirements is a (req Str Str Course )  
(course) :A Course is a (course (listof Subject)) 
(subject):A subject is (subject Str Str Str Str (list Str Str) (listof Char))  
Example req: 
 
(req "09:30" "14:30"  
       (course (list (subject "MATH" "136" empty empty '(1030 1120) empty) 
                     (subject "CS" "136" empty '("Kharal,Rosina") empty empty) 
                     (subject "BIOL" "239" empty empty empty empty) 
                     (subject "EMLS" "102R" empty empty '(1300 1420) empty) 
                     (subject "MATH" "138" empty empty empty empty))))  
 please use (get-prof ) fuction to get a professor's name, for example:   
  (prof "CS" "116") 
