package u02


import u03.extensionmethods.Sequences.Sequence.foldLeft

import scala.compiletime.ops.int.+

object Modules extends App:

  // An ADT: type + module
  enum Person:
    case Student(name: String, year: Int)
    case Teacher(name: String, course: String)

  object Person:
    def name(p: Person): String = p match
      case Student(n, _) => n
      case Teacher(n, _) => n

  println(Person.name(Person.Student("mario", 2015)))

  import Person.*

  println(name(Student("mario", 2015)))

  // a method outside the Person module
  def isStudent(p: Person): Boolean = p match
    case Student(_, _) => true
    case _ => false

  println(isStudent(Student("mario", 2015)))

  import u03.extensionmethods.Sequences.*

  extension (s: Sequence[Person])

    def courseOfTeachers(): Sequence[String] =
      s.filter((p: Person) => !isStudent(p)).map((a: Person) => a.asInstanceOf[Teacher].course)

  end extension

  def courseOfTeachers2(s: Sequence[Person]): Sequence[String] =
    s.filter {
      case Teacher(_, _) => true
      case _: Person => false
    }.map {
      case Teacher(_, course) => course
      case _ => ""
    }

  def totalCourseOfTeachers(s: Sequence[Person]): Int =
    s.filter((p: Person) => !isStudent(p)).map((a: Person) => a.asInstanceOf[Teacher].course.length).foldLeft(0, _ + _)

end Modules
