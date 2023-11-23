package org.dwcj.concern;

import com.google.gson.reflect.TypeToken;
import java.lang.reflect.Type;
import org.dwcj.component.Component;

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
  public T setProperty(String property, Object value);

  /**
   * Retrieves the value of the given property.
   *
   * @param <V> the type of the property value returned
   * @param property the key of the property
   * @param typeOfV the type to cast the returned value to. The method will attempt to convert the
   *        value returned by the client to the appropriate Java data type. However, that this
   *        conversion depends on various factors, and although it usually works out of the box,
   *        there could be cases where it doesn't. So, providing a hint for the data type in such
   *        situations is necessary.
   *
   * @return the retrieved value.
   * @since 23.06
   */
  public <V> V getProperty(String property, Type typeOfV);

  /**
   * Retrieves the value of the given property.
   *
   * @param <V> the type of the property value returned
   * @param classOfV the type to cast the returned value to. The method will attempt to convert the
   *        value returned by the client to the appropriate Java data type. However, that this
   *        conversion depends on various factors, and although it usually works out of the box,
   *        there could be cases where it doesn't. So, providing a hint for the data type in such
   *        situations is necessary.
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
