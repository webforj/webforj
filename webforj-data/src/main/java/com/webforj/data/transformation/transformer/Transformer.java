package com.webforj.data.transformation.transformer;

import com.webforj.data.transformation.TransformationException;
import java.util.function.Function;

/**
 * Represents a transformer that can be used to transform values between the model and the component
 * presentation.
 *
 * @param <ComponentValueT> The type of the view value.
 * @param <ModelValueT> The type of the model value.
 *
 * @since 24.01
 * @author Hyyan Abo Fakher
 */
public interface Transformer<ComponentValueT, ModelValueT> {

  /**
   * Transforms the given view value to the model value.
   *
   * @param viewValue The view value.
   * @return The model value.
   * @throws TransformationException If there is a problem with the transformation.
   */
  public ModelValueT transformToModel(ComponentValueT viewValue);

  /**
   * Transforms the given model value to the view value.
   *
   * @param modelValue The model value.
   * @return The component value.
   * @throws TransformationException If there is a problem with the transformation.
   */
  public ComponentValueT transformToComponent(ModelValueT modelValue);

  /**
   * Returns a transformer that uses the given functions to transform the values.
   *
   * @param <ComponentValueT> The type of the view value.
   * @param <ModelValueT> The type of the model value.
   *
   * @param toModel The function to use to transform the view value to the model value.
   * @param toView The function to use to transform the model value to the view value.
   *
   * @return The transformer.
   */
  public static <ComponentValueT, ModelValueT> Transformer<ComponentValueT, ModelValueT> of(Function<ComponentValueT, ModelValueT> toModel, Function<ModelValueT, ComponentValueT> toView) {
    return new Transformer<ComponentValueT, ModelValueT>() {
      @Override
      public ModelValueT transformToModel(ComponentValueT viewValue) {
        try {
          return toModel.apply(viewValue);
        } catch (Exception e) {
          throw new TransformationException("Error transforming component value to model value", e);
        }
      }

      @Override
      public ComponentValueT transformToComponent(ModelValueT modelValue) {
        try {
          return toView.apply(modelValue);
        } catch (Exception e) {
          throw new TransformationException("Error transforming model value to component value", e);
        }
      }
    };
  }
}

