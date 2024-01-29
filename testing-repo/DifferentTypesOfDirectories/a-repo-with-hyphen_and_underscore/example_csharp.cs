// This file es CRLF enforced in .gitattributes
using System;
using System.Collections.Generic;
using System.IO;

namespace Augmented_Vs_Plugin.Utils
{
  /// <summary>
  /// This is an example file to be used for testing the ac-parser only 😂 汉字.
  /// </summary>
  public static class CSharpParser
  {
    /// <summary> A basic example of docstring </summary>
    public static int function1(string fullPath, List<int> offset)
    {
      // inline comment with multibyte chars 😂 汉字.
      return 1;
    }

    /// <summary>
    /// This is another example of a correct docstring
    /// </summary>
    /// <param name="fullPath">The full path to a file.</param>
    /// <param name="offset">A list containing the offset definition.</param>
    /// <returns>The code preview.</returns>
    public string function2()
    {
      int i = 10;
      while (i > 0) i--;
      return "literal";
    }

    /** <summary> docstrings can use the Java format too. </summary> */
    public static bool function3()
    {
      return true;
    }

    /**
        <summary> docstrings can use the Java format too. </summary>
    */
    public static int function4()
    {
      return 5;
    }

    /// <summary>
    /// Docstring above an Attribute. <see cref="IOneGetContext"/>
    /// </summary>
    /// <param name="a">first param</param>
    /// <param name="b">second param</param>
    /// <returns></returns>
    [HttpPost]
    public static IEnumerable<String> function5(int a, int b)
    {
      return new List<String> { "" };
    }

    // This is not a valid docstring
    public static string function6(string path)
    {
      return "test";
    }

    // <summary> This is not a valid docstring </summary>
    public static string function7(string path)
    {
      return "test";
    }

    /// <summary>  This is not a valid docstring
    public static void function8()
    {
      Console.WriteLine("test");
    }

    /// <summary>docstring 😂 morè téxt 汉字* </summary> ///This is extra comment
    public static void weirdChar() {
      Console.WriteLine("test");
    }

    // Inner class
    public class Inner_class {

        // Method of inner class
        public void innerClassMethod()
        {
            Console.WriteLine("Inner class Method");
        }
    }

    /// <summary>
    ///   <c>GetUsername</c> gets the username associated.
    ///   <para>
    ///     <see cref="GenericType{T}"/>
    ///   </para>
    ///   <returns>A string</returns>
    /// </summary>
    public void functionWithNestedTags() {
        Console.WriteLine("functionWithNestedTags");
    }
  }
  interface Animal
    {
      void animalSound();
      float normalMethod() { return 3.5; }
    }
}
