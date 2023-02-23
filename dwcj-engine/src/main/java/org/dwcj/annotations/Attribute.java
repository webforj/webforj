package org.dwcj.annotations;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * This annotation is used to set the attributes of the meta/styles and
 * javascript annotations.
 * 
 * <pre>
 * {@code
 * @AppMeta(name = "custom name", content = "custom content", attributes = {
 *  @Attribute(name = "custom-attribute", value = "custom attribute value"),
 *  @Attribute(name = "custom-attribute2", value = "custom attribute value2")
 * })
 * }
 * </pre>
 * 
 * @see AppMeta
 * 
 * @author Hyyan Abo Fakher
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
@Inherited
@Documented
public @interface Attribute {

  /** 
   * The name of the attribute
   * 
   * @return the name of the attribute
   **/
  String name();

  /** 
   * The value of the attribute
   * 
   * @return the value of the attribute
   **/
  String value();
}
