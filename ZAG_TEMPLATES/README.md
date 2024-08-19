# TEMPLATES

---

## Object Oriented Report

Object structuring of reporting is a way of structuring your programs using the concepts of classes and objects. The use of such components differs from classical ABAP programming which has always been procedural until now.

<br>

Consequently, the use of perform and global variables is replaced by the use of objects, interfaces and methods.
Error management is also different as it introduces the use of exception classes specifically designed to structure the occurrence of unexpected situations in a more complex way.

<br>

The following are the macro components of an object report:
- [Main Program](https://github.com/avorio-dev/S4ZAG/blob/main/ZAG_TEMPLATES/OO_REPORT/ZAG_OO_REPORT.abap)

- [Definition Include](https://github.com/avorio-dev/S4ZAG/blob/main/ZAG_TEMPLATES/OO_REPORT/ZAG_OO_REPORT_DEF.abap)

- [Implementation Include](https://github.com/avorio-dev/S4ZAG/blob/main/ZAG_TEMPLATES/OO_REPORT/ZAG_OO_REPORT_IMP.abap)

---

## ALV LVC Template - ALV Dynpro-Based

Dynpro based ALV is an ALV type which basically offers user-side functions and information enclosed into dynpros. THe logic of your report will be based on a stack which each step is a dynpro.

**NB To use this template you will need to follow mandatory activities, otherwise it will not works.**

Each activity is listed at the beginning of the template code.
If you wish, you can customize your ALV following also the optional activities.