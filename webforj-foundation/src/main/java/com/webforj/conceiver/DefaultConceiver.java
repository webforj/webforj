package com.webforj.conceiver;

import com.webforj.App;
import com.webforj.component.Component;
import com.webforj.component.event.ComponentEvent;
import com.webforj.conceiver.exception.ConceiverException;
import com.webforj.exceptions.WebforjAppInitializeException;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Modifier;
import java.util.Map;

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
      // Check if the class is a non-static inner class
      if (classOfT.getEnclosingClass() != null && !Modifier.isStatic(classOfT.getModifiers())) {
        // Get the outer class instance
        Object outerInstance = classOfT.getEnclosingClass().getDeclaredConstructor().newInstance();
        Constructor<T> constructor = classOfT.getDeclaredConstructor(classOfT.getEnclosingClass());
        return constructor.newInstance(outerInstance);
      } else {
        // Handle regular classes and static inner classes
        Constructor<T> constructor = classOfT.getDeclaredConstructor();
        return constructor.newInstance();
      }
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

  /**
   * {@inheritDoc}
   */
  @Override
  public <E extends ComponentEvent<?>> E getComponentEvent(Component component,
      Class<?> eventClass, Map<String, Object> data) {
    E event = null;

    try {
      Constructor<?>[] constructors = eventClass.getDeclaredConstructors();
      for (Constructor<?> constructor : constructors) { // NOSONAR
        constructor.setAccessible(true); // NOSONAR
        Class<?>[] parameterTypes = constructor.getParameterTypes();

        // is not inner class
        if (parameterTypes.length == 2 && Component.class.isAssignableFrom(parameterTypes[0])
            && parameterTypes[1] == Map.class) {
          event = (E) constructor.newInstance(component, data); // NOSONAR
          break;
        }
        // else if inner class
        else if (parameterTypes.length == 3 && Component.class.isAssignableFrom(parameterTypes[0])
            && Component.class.isAssignableFrom(parameterTypes[1])
            && parameterTypes[2] == Map.class) {
          event = (E) constructor.newInstance(component, component, data); // NOSONAR

          break;
        }
      }
    } catch (InstantiationException | IllegalAccessException | IllegalArgumentException
        | InvocationTargetException ev) {
      throw new ConceiverException(
          "Failed to create an instance of the event class [" + eventClass.getName() + "]", ev);
    }

    return event;
  }
}
