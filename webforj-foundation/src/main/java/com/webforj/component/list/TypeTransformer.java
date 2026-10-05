package com.webforj.component.list;

import com.webforj.data.transformation.TransformationException;
import com.webforj.data.transformation.transformer.Transformer;

/**
 * Represents a transformer that can be used to cast values between the model and the component.
 *
 * @param <ViewT> The type of the view value.
 * @param <ModelT> The type of the model value.
 *
 * @author Hyyan Abo Fakher
 * @since 24.01
 */
class TypeTransformer<ViewT, ModelT> implements Transformer<ViewT, ModelT> {

  /**
   * {@inheritDoc}
   */
  @Override
  public ModelT transformToModel(ViewT viewValue) {
    try {
      return (ModelT) viewValue;
    } catch (Exception e) {
      throw new TransformationException("Failed to cast view value to model type.");
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public ViewT transformToComponent(ModelT modelValue) {
    try {
      return (ViewT) modelValue;
    } catch (Exception e) {
      throw new TransformationException("Failed to cast model value to view type.");
    }
  }
}
