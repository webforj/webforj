package com.webforj.data.transformation.transformer;

import com.webforj.data.transformation.TransformationException;
import java.util.function.Function;

/**
 * Represents a transformer that can be used to transform values between the model and the component
 * presentation.
 *
 * @param <ViewT> The type of the view value.
 * @param <ModelT> The type of the model value.
 *
 * @since 24.01
 * @author Hyyan Abo Fakher
 */
public interface Transformer<ViewT, ModelT> {

  /**
   * Transforms the given view value to the model value.
   *
   * @param viewValue The view value.
   * @return The model value.
   * @throws TransformationException If there is a problem with the transformation.
   */
  public ModelT transformToModel(ViewT viewValue);

  /**
   * Transforms the given model value to the view value.
   *
   * @param modelValue The model value.
   * @return The component value.
   * @throws TransformationException If there is a problem with the transformation.
   */
  public ViewT transformToComponent(ModelT modelValue);

  /**
   * Returns a transformer that uses the given functions to transform the values.
   *
   * @param <ViewT> The type of the view value.
   * @param <ModelT> The type of the model value.
   *
   * @param toModel The function to use to transform the view value to the model value.
   * @param toView The function to use to transform the model value to the view value.
   *
   * @return The transformer.
   */
  public static <ViewT, ModelT> Transformer<ViewT, ModelT> of(Function<ViewT, ModelT> toModel,
      Function<ModelT, ViewT> toView) {
    return new Transformer<ViewT, ModelT>() {
      @Override
      public ModelT transformToModel(ViewT viewValue) {
        try {
          return toModel.apply(viewValue);
        } catch (Exception e) {
          throw new TransformationException("Error transforming component value to model value", e);
        }
      }

      @Override
      public ViewT transformToComponent(ModelT modelValue) {
        try {
          return toView.apply(modelValue);
        } catch (Exception e) {
          throw new TransformationException("Error transforming model value to component value", e);
        }
      }
    };
  }
}

