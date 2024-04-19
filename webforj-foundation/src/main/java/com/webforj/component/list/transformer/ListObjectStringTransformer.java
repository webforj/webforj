package com.webforj.component.list.transformer;

import com.webforj.data.transformation.TransformationException;
import com.webforj.data.transformation.transformer.Transformer;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

/**
 * A transformer that transforms a list of objects to a list of strings and vice versa.
 *
 * @since 24.01
 * @author Hyyan Abo Fakher
 */
public class ListObjectStringTransformer implements Transformer<List<Object>, List<String>> {

  /**
   * {@inheritDoc}
   */
  @Override
  public List<String> transformToModel(List<Object> componentValue) {
    try {
      return componentValue == null ? Collections.emptyList()
          : componentValue.stream().map(String::valueOf).toList();
    } catch (Exception e) {
      throw new TransformationException(e);
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public List<Object> transformToComponent(List<String> modelValue) {
    try {
      return modelValue == null ? Collections.emptyList() : modelValue.stream().map(item -> {
        try {
          return Object.class.cast(item);
        } catch (ClassCastException e) {
          return null;
        }
      }).filter(Objects::nonNull).toList();
    } catch (Exception e) {
      throw new TransformationException(e);
    }
  }
}
