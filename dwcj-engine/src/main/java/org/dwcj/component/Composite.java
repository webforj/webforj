package org.dwcj.component;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import org.dwcj.PendingResult;
import org.dwcj.component.window.Window;

/**
 * This abstract class defines the blueprint for a composite component, which is a high-level
 * construct designed to encapsulate and manage a specific {@code Component} instance. It requires
 * any extending subclass to specify the type of {@code Component} it intends to manage, ensuring a
 * subclass of {@code Composite} is intrinsically linked to its underlying {@code Component}.
 *
 * <p>
 * The design of the {@code Composite} class enforces a strong association between a composite
 * component and its underlying component. This relationship is crucial for the execution of system
 * operations that depend on the composite structure.
 * </p>
 *
 * <p>
 * Access to the internal component hierarchy managed by a composite is provided by the
 * {@link #getBoundComponent()} method. By default, {@code Composite} utilizes the generic type
 * parameter of its subclass to identify and instantiate the bound component type, provided that the
 * component class has a parameter-less constructor. Alternatively, subclasses may customize the
 * component initialization process by overriding the {@link #doInitBoundComponent()} method.
 * </p>
 *
 * @param <T> the class type of the {@code Component} that is managed by this {@code Composite}.
 *
 * @author Hyyan Abo Fakher
 * @since 23.06
 */
public abstract class Composite<T extends Component> extends Component {
  private final T component;

  /**
   * Creates a new {@code Composite} instance and initializes the bound {@code Component} instance.
   */
  protected Composite() {
    super();
    component = initBoundComponent();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String geClientComponentId() {
    return getBoundComponent().geClientComponentId();
  }

  /**
   * Initializes the bound {@code Component} and returns an instance of it. This method calls the
   * {@link #doInitBoundComponent} method which can be implemented by the developer to provide the
   * specific bound component in case the default implementation is not sufficient.
   *
   * <p>
   * Developers should not call this method directly. Instead, they should implement the
   * {@link #doInitBoundComponent} method to provide the specific bound component if necessary and
   * use the {@link #getBoundComponent} method to get the bound component.
   * </p>
   *
   * <p>
   * This method will create the bound component only if it has not been created yet or if the
   * component was created before but destroyed later.
   * </p>
   *
   * @return An instance of the bound {@code Component} associated with this {@code Composite}
   */
  protected T initBoundComponent() {
    if (component == null) {
      Class<T> componentClass = findBoundComponentClass();
      Constructor<T> constructor = findNoArgsConstructor(componentClass);
      return invokeBoundComponentConstructor(constructor);
    }

    return component;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isDestroyed() {
    return getBoundComponent().isDestroyed();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isAttached() {
    return getBoundComponent().isAttached();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void onCreate(Window window) {
    window.add(getBoundComponent());
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void onDestroy() {
    if (component != null) {
      component.destroy();
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public PendingResult<Component> whenAttached() {
    return getBoundComponent().whenAttached();
  }

  /**
   * Gets the underlying {@code Component} that this {@code Composite} component is associated with.
   *
   * @return An instance of the underlying {@code Component} associated with this {@code Composite}
   *         component.
   */
  protected final T getBoundComponent() {
    return component;
  }

  private Class<T> findBoundComponentClass() {
    Class<?> currentClass = getClass();

    while (currentClass.getSuperclass() != null) {
      Type genericSuperclass = currentClass.getGenericSuperclass();

      if (genericSuperclass instanceof ParameterizedType) {
        ParameterizedType parameterizedType = (ParameterizedType) genericSuperclass;
        Type[] typeArguments = parameterizedType.getActualTypeArguments();
        if (typeArguments.length > 0) {
          @SuppressWarnings("unchecked")
          Class<T> contentClass = (Class<T>) typeArguments[0];
          return contentClass;
        }
      }

      // move up the hierarchy
      currentClass = currentClass.getSuperclass();
    }

    throw new IllegalStateException(
        "Cannot determine the generic type for the Composite component.");
  }

  private Constructor<T> findNoArgsConstructor(Class<T> boundComponentClass) {
    try {
      return boundComponentClass.getDeclaredConstructor();
    } catch (NoSuchMethodException e) {
      throw new IllegalStateException("The bound component class " + boundComponentClass.getName()
          + " does not have a no-arg constructor.", e);
    }
  }

  private T invokeBoundComponentConstructor(Constructor<T> constructor) {
    try {
      constructor.setAccessible(true); // NOSONAR
      return constructor.newInstance();
    } catch (InstantiationException e) {
      throw new IllegalStateException(
          "Cannot instantiate bound component class. " + constructor.getDeclaringClass().getName()
              + ". Is it an abstract class or an interface?",
          e);
    } catch (IllegalAccessException e) {
      throw new IllegalStateException("Cannot access the constructor of bound component class.", e);
    } catch (InvocationTargetException e) {
      throw new IllegalStateException("Constructor threw an exception.", e.getCause());
    }
  }
}
