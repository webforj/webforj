package com.webforj.test;

import com.webforj.annotation.JavaScript;
import com.webforj.annotation.StyleSheet;

/**
 * Test application with webforJ asset annotations.
 */
@StyleSheet("webserver://css/test.css")
@JavaScript("ws://js/test.js")
public class TestApp {

  public static void main(String[] args) {
    System.out.println("Test application");
  }
}
