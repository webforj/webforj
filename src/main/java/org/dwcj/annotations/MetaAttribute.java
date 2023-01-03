package org.dwcj.annotations;

/**
 * This annotation is used to set the attributes of the meta tag. It can be used
 * multiple times to set multiple attributes. The attributes are optional.
 * 
 * <p>
 * Example:
 * 
 * <pre>
 * {@code
 * &#64;AppMeta(name = "custom name", content = "custom content", attributes = {
 *  &#64;MetaAttribute(name = "custom-attribute", value = "custom attribute value"),
 *  &#64;MetaAttribute(name = "custom-attribute2", value = "custom attribute value2")
 * })
 * }
 * </pre>
 * 
 * @author Hyyan Abo Fakher
 */
public @interface MetaAttribute {

  /** The name of the attribute */
  String name();

  /** The value of the attribute */
  String value();
}
