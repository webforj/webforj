package com.webforj.conceiver;

import com.webforj.App;
import com.webforj.component.Component;
import com.webforj.conceiver.exception.ConceiverException;
import com.webforj.exceptions.WebforjAppInitializeException;

/**
 * The default implementation of the {@link Conceiver}.
 *
 * @since 24.12
 * @author Hyyan Abo Fakher
 */
public class DefaultConceiver implements Conceiver {

  /**
   * {@inheritDoc}
   */
  @Override
  public <T> T get(Class<T> classOfT) {
    try {
      return classOfT.getDeclaredConstructor().newInstance();
    } catch (Exception ex) {
      throw new ConceiverException(
          "Failed to create an instance of the class [" + classOfT.getName() + "]", ex);
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public App getApplication(Class<? extends App> appClass) throws WebforjAppInitializeException {
    return get(appClass);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public <T extends Component> T getComponent(Class<T> classOfT) {
    return get(classOfT);
  }
}
