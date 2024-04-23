package com.webforj.component.list;

import com.webforj.data.transformation.TransformationException;
import com.webforj.data.transformation.transformer.Transformer;

/**
 * Represents a transformer that can be used to cast values between the model and the component.
 *
 * @param <CV> The type of the view value.
 * @param <MV> The type of the model value.
 *
 * @author Hyyan Abo Fakher
 * @since 24.01
 */
class TypeTransformer<CV, MV> implements Transformer<CV, MV> {

  /**
   * {@inheritDoc}
   */
  @Override
  public MV transformToModel(CV viewValue) {
    try {
      return (MV) viewValue;
    } catch (Exception e) {
      throw new TransformationException("Failed to cast view value to model type.");
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public CV transformToComponent(MV modelValue) {
    try {
      return (CV) modelValue;
    } catch (Exception e) {
      throw new TransformationException("Failed to cast model value to view type.");
    }
  }
}
