package com.webforj.data.binding.annotation;

import com.webforj.data.transformation.transformer.Transformer;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Annotation to specify the transformer that should be used to convert between the model and the
 * view values.
 *
 * @author Hyyan Abo Fakher
 * @since 24.01
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.FIELD)
public @interface UseTransformer {
  /**
   * The transformer class to use for transforming the value. The transformer class must implement
   * the {@link Transformer} interface.
   */
  Class<? extends Transformer<?, ?>> value();

  /**
   * The message to be used in case of transformation failure.
   */
  String message() default "";
}
