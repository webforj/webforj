package com.webforj.spring.routescan;

import java.util.concurrent.atomic.AtomicBoolean;

/**
 * Holds whether the static initializer of {@link RecordedClass} ran.
 */
public final class StaticInitializerRecord {

  /**
   * Set once the static initializer of {@link RecordedClass} ran.
   */
  public static final AtomicBoolean RAN = new AtomicBoolean();

  private StaticInitializerRecord() {}

  /**
   * A class whose static initializer sets {@link StaticInitializerRecord#RAN}.
   */
  public static final class RecordedClass {
    static {
      RAN.set(true);
    }

    private RecordedClass() {}
  }
}
