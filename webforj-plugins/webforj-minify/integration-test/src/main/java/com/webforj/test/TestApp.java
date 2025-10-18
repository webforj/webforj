package com.webforj.test;

import com.webforj.annotation.JavaScript;
import com.webforj.annotation.StyleSheet;
import java.util.logging.Logger;

/**
 * Test application with webforJ asset annotations.
 */
@StyleSheet("webserver://css/test.css")
@JavaScript("ws://js/test.js")
public class TestApp {
  private static final Logger LOGGER = Logger.getLogger(TestApp.class.getName());

  private TestApp() {
    // Private constructor to hide implicit public one
  }

  public static void main(String[] arguments) {
    LOGGER.info("Test application");
  }
}
