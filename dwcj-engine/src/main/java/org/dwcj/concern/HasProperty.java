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
   * @param typeOfT the type of the property value returned to convert the value to.
   *
   * @return the retrieved value.
   * @since 23.06
   */
  public <V> V getProperty(String property, Type typeOfT);

  /**
   * Retrieves the value of the given property.
   *
   * @param <V> the type of the property value returned
   * @param classOfT the type of the property value returned to convert the value to.
   *
   * @return the retrieved value.
   * @since 23.06
   */
  public default <V> V getProperty(String property, Class<V> classOfT) {
    return getProperty(property, TypeToken.get(classOfT).getType());
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
