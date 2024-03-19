package com.webforj.concern;

import com.google.gson.reflect.TypeToken;
import com.webforj.component.Component;
import com.webforj.component.ComponentUtil;
import java.lang.reflect.Type;

/**
 * The HasProperty interface is used to set and retrieve properties of a component.
 *
 * <p>
 * The properties are key-value pairs that can be used to configure the client component. Note the
 * we always reach the client component to set or retrieve a property.
 * </p>
 *
 * @param <T> the type of the component that implements this interface.
 *
 * @author Hyyan Abo Fakher
 * @since 23.05
 */
public interface HasProperty<T extends Component> {

  /**
   * Sets a property value.
   *
   * @param property the key of the property
   * @param value the value to be set
   *
   * @return the component itself.
   */
  public default T setProperty(String property, Object value) {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof HasProperty) {
      ((HasProperty<?>) component).setProperty(property, value);
      return (T) this;
    }

    throw new UnsupportedOperationException("The component does not support properties");
  }

  /**
   * Retrieves the value of the given property.
   *
   * @param <V> the type of the property value returned
   * @param property the key of the property
   * @param typeOfV the type to cast the returned value to. The method will attempt to convert the
   *        value returned by the client to the appropriate Java data type. However, this conversion
   *        depends on various factors, and although it usually works out of the box, there may be
   *        cases where it doesn't. So, providing a specified data type in such situations is
   *        necessary.
   *
   * @return the retrieved value.
   * @since 23.06
   */
  public default <V> V getProperty(String property, Type typeOfV) {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof HasProperty) {
      return (V) ((HasProperty<?>) component).getProperty(property, typeOfV);
    }

    throw new UnsupportedOperationException("The component does not support properties");
  }

  /**
   * Retrieves the value of the given property.
   *
   * @param <V> the type of the property value returned
   * @param classOfV the type to cast the returned value to. The method will attempt to convert the
   *        value returned by the client to the appropriate Java data type. However, this conversion
   *        depends on various factors, and although it usually works out of the box, there may be
   *        cases where it doesn't. So, providing a specified data type in such situations is
   *        necessary.
   *
   * @return the retrieved value.
   * @since 23.06
   */
  public default <V> V getProperty(String property, Class<V> classOfV) {
    return getProperty(property, TypeToken.get(classOfV).getType());
  }

  /**
   * Retrieves the value of the given property.
   *
   * @param property the key of the property
   * @return the retrieved value.
   */
  public default Object getProperty(String property) {
    return getProperty(property, Object.class);
  }
}
