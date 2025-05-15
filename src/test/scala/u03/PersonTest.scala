package u03

import extensionmethods.Sequences.*
import org.junit.*
import org.junit.Assert.*
import Sequence.*
import u02.Modules.{Person, courseOfTeachers, courseOfTeachers2, totalCourseOfTeachers}
import u02.Modules.Person.*

class PersonTest:

  val sequence: Sequence[Person] = Cons(Student("Mario", 2000), Cons(Teacher("Ghini", "SISTEMIOPERATIVI"), Cons(Teacher("Rossi", "Chimica"), Nil())))

  @Test def testCoursesOfTeachers() =
    assertEquals(Cons("SISTEMIOPERATIVI", Cons("Chimica", Nil())), sequence.courseOfTeachers())

  @Test def testCoursesOfTeachers2() =
    assertEquals(Cons("SISTEMIOPERATIVI", Cons("Chimica", Nil())), courseOfTeachers2(sequence))

  @Test def testCoursesCount() =
    assertEquals(2, totalCourseOfTeachers(sequence))
