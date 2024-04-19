package com.webforj.data.transformation.transformer;

import com.webforj.data.transformation.TransformationException;
import java.util.function.Function;

/**
 * Represents a transformer that can be used to transform values between the model and the component
 * presentation.
 *
 * @param <CV> The type of the view value.
 * @param <MV> The type of the model value.
 *
 * @since 24.01
 * @author Hyyan Abo Fakher
 */
public interface Transformer<CV, MV> {

  /**
   * Transforms the given view value to the model value.
   *
   * @param viewValue The view value.
   * @return The model value.
   * @throws TransformationException If there is a problem with the transformation.
   */
  public MV transformToModel(CV viewValue);

  /**
   * Transforms the given model value to the view value.
   *
   * @param modelValue The model value.
   * @return The component value.
   * @throws TransformationException If there is a problem with the transformation.
   */
  public CV transformToComponent(MV modelValue);

  /**
   * Returns a transformer that uses the given functions to transform the values.
   *
   * @param toModel The function to use to transform the view value to the model value.
   * @param toView The function to use to transform the model value to the view value.
   *
   * @return The transformer.
   */
  public default Transformer<CV, MV> of(Function<CV, MV> toModel, Function<MV, CV> toView) {
    return new Transformer<CV, MV>() {
      @Override
      public MV transformToModel(CV viewValue) {
        return toModel.apply(viewValue);
      }

      @Override
      public CV transformToComponent(MV modelValue) {
        return toView.apply(modelValue);
      }
    };
  }
}

