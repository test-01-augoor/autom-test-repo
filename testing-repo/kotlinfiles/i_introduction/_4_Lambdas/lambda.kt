package i_introduction._4_Lambdas

import data.Person
import util.TODO
import util.doc4
import java.util.*

/**
 * Created by hardik.trivedi on 14/05/18.
 */


fun lambda0(collection: Array<Int>): List<Int> = todoTasKLambda0(collection)

fun todoTasKLambda0(collection: Array<Int>): Nothing = TODO(
        """
            Write the code in Kotlin in such a way that lambda.kt->todoTasKLambda0()
            return the only even numbers out of the list passed to it.
    """,
        documentation = doc4())

fun lambda1(collection: Array<String>): List<String> = todoTaskLambda1(collection)


fun todoTaskLambda1(collection: Array<String>): Nothing = TODO(
        """
            Write the code in Kotlin in such a way that lambda.kt->todoTaskLambda1()
            returns all those words which starts with H(Case insensitive) and word length is greater than 4
    """,
        documentation = doc4())

fun lambda2(collection: List<Int>): List<Int> = todoTaskLambda2(collection)

fun todoTaskLambda2(collection: List<Int>): Nothing = TODO(
        """
            Write the code in Kotlin in such a way that lambda.kt->todoTaskLambda2()
            returns all the numbers in descending order.
    """,
        documentation = doc4())

fun lambda3(collection: List<Person>): List<Person> = todoTaskLambda3(collection)

fun todoTaskLambda3(collection: List<Person>): Nothing = TODO(
        """
            Write the code in Kotlin in such a way that lambda.kt->todoTaskLambda3()
            returns all the persons in ascending order sort by their name
    """,
        documentation = doc4())

fun lambda4(collection: List<Person>): List<Person> = todoTaskLambda4(collection)

fun todoTaskLambda4(collection: List<Person>): Nothing = TODO(
        """
            Write the code in Kotlin in such a way that lambda.kt->todoTaskLambda4()
            returns all the persons whose age is greater than 30 and sort by their name in ascending order
    """,
        documentation = doc4())

fun lambda5(collection: List<Int>): List<Int> = todoTaskLambda5(collection)

fun todoTaskLambda5(collection: List<Int>): Nothing = TODO(
        """
            Write the code in Kotlin in such a way that lambda.kt->todoTaskLambda5()
            which doubles all the numbers in the given list
    """,
        documentation = doc4())