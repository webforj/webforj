package com.webforj.conceiver;

import com.webforj.App;
import com.webforj.component.Component;
import com.webforj.conceiver.exception.ConceiverException;
import com.webforj.exceptions.WebforjAppInitializeException;

/**
 * A factory for creating instances of classes.
 *
 * <p>
 * Applications that want to provides conceivers should implement this interface. The implementing
 * class must be registered in the {@code com.webforj.conceiver.Conceiver} file located in the
 * {@code META-INF/services} directory.
 * </p>
 *
 * @since 24.12
 * @author Hyyan Abo Fakher
 */
public interface Conceiver {

  /**
   * Get the instance of the given class.
   *
   * @param <T> the type of the class.
   * @param classOfT the class.
   *
   * @return the instance of the class.
   * @throws ConceiverException if an error occurs while creating the instance.
   */
  <T> T get(Class<T> classOfT);

  /**
   * Get the instance of the given application class.
   *
   * @param appClass the application class.
   * @return the instance of the application.
   * @throws WebforjAppInitializeException if an error occurs while creating the instance.
   */
  App getApplication(Class<? extends App> appClass) throws WebforjAppInitializeException;

  /**
   * Create a component instance of the given class.
   *
   * @param <T> the type of the component.
   * @param classOfT the class of the component.
   * @return the instance of the component.
   */
  <T extends Component> T getComponent(Class<T> classOfT);
}
