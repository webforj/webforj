package com.webforj.component.list.transformer;

import com.webforj.data.transformation.TransformationException;
import com.webforj.data.transformation.transformer.Transformer;

/**
 * A transformer that transforms an object to a string and vice versa.
 *
 * @since 24.01
 * @author Hyyan Abo Fakher
 */
public class ObjectStringTransformer implements Transformer<Object, String> {

  /**
   * {@inheritDoc}
   */
  @Override
  public String transformToModel(Object componentValue) {
    try {
      return componentValue == null ? "" : String.valueOf(componentValue);
    } catch (Exception e) {
      throw new TransformationException(e);
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Object transformToComponent(String modelValue) {
    try {
      return modelValue == null || modelValue.isEmpty() ? null : modelValue;
    } catch (Exception e) {
      throw new TransformationException(e);
    }
  }
}
