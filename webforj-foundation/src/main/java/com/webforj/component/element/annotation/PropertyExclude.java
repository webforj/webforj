package com.webforj.component.element.annotation;

import com.webforj.component.element.PropertyDescriptorTester;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Annotation to hint the {@link PropertyDescriptorTester} to ignore the property.
 *
 * @author Hyyan Abo Fakher
 * @since 24.10
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.FIELD)
public @interface PropertyExclude {
}
