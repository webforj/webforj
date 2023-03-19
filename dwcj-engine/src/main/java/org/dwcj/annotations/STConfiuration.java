package org.dwcj.annotations;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * An annotation to define the configuration to manipulate the global
 * {@link org.dwcj.environment.StringTable}. 
 * 
 * <p>
 * The configuration is a key/value pair that will be passed to the global
 * {@link org.dwcj.environment.StringTable}.
 * </p>
 * 
 * <p>
 * This annotation can be used multiple times to define multiple configuration
 * and it can be used at the Application or the controls level. But application
 * level configurations will override the controls level configurations.
 * </p>
 * 
 * <pre>
 * {@code
 *  &#64;Configuration(key = "DEBUG", value = "1")
 * }
 * </pre>
 * 
 * @see org.dwcj.environment.StringTable
 * 
 * @author Hyyan Abo Fakher
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@Repeatable(STConfiuration.Container.class)
@Inherited
@Documented
public @interface STConfiuration {
  /**
   * The configuration key
   * 
   * @return the configuration key
   */
  String key();

  /**
   * The configuration value
   * 
   * @return the configuration value
   **/
  String value();

  /**
   * A container for the {@link STConfiuration} annotation
   * 
   * @see STConfiuration
   * @author Hyyan Abo Fakher
   */
  @Target(ElementType.TYPE)
  @Retention(RetentionPolicy.RUNTIME)
  @Inherited
  @Documented
  public @interface Container {
    /**
     * A container for the {@link STConfiuration} annotation
     * 
     * @return the {@link STConfiuration} annotations
     */
    STConfiuration[] value();
  }
}
