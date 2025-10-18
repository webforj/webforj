package com.webforj.minify.common;

/**
 * Exception thrown when minification fails.
 */
public class MinificationException extends Exception {

  public MinificationException(String message) {
    super(message);
  }

  public MinificationException(String message, Throwable cause) {
    super(message, cause);
  }
}
