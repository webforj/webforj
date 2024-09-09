package com.webforj.component;

import com.webforj.PendingResult;
import com.webforj.component.window.Window;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;

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
 * component initialization process by overriding the {@link #initBoundComponent()} method.
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
  public String getClientComponentId() {
    return getBoundComponent().getClientComponentId();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Component setName(String name) {
    getBoundComponent().setName(name);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getName() {
    return getBoundComponent().getName();
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
  public PendingResult<Component> whenAttached() {
    return getBoundComponent().whenAttached();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Window getWindow() {
    return getBoundComponent().getWindow();
  }

  /**
   * Initializes the bound {@code Component} and returns an instance of it. This method can be
   * implemented by the developer to provide the specific bound component in case the default
   * implementation is not sufficient.
   *
   * <p>
   * Developers should not call this method directly. Instead, they should implement the method to
   * provide the specific bound component if necessary and use the {@link #getBoundComponent} method
   * to get the bound component.
   * </p>
   *
   * <p>
   * This method will create the bound component only if it has not been created yet.
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
  protected void onCreate(Window window) {
    window.add(getBoundComponent());
    onDidCreate(component);
  }

  /**
   * Called immediately after the bound {@link Component} has been created and added to the window.
   * Subclasses must implement this method to perform additional setup or initialization specific to
   * the component. This is an opportune moment to add child components to the bound component,
   * apply settings, and make the component ready for interaction within the application.
   *
   * <p>
   * This method is part of the component lifecycle and is invoked after the base class's
   * {@code onCreate} method has completed the initial creation process. It allows the subclass to
   * further refine the behavior and presentation of the component before it begins to handle user
   * interactions or data processing.
   * </p>
   *
   * @param container The instance of the component that has been created.
   */
  protected void onDidCreate(T container) {
    // pass
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void onDestroy() {
    if (component != null) {
      component.destroy();
    }

    onDidDestroy();
  }

  /**
   * Called immediately after the container has been destroyed. Subclasses can implement this method
   * to clean up resources or perform any other necessary teardown operations. This method serves as
   * a notification that the component is no longer in a usable state.
   *
   * <p>
   * This method is part of the component lifecycle and is invoked after the component has completed
   * the destruction process, ensuring that any final actions that depend on the component being in
   * a partially intact state can be completed.
   * </p>
   *
   * <p>
   * It is important to note that after this method is called, the component and its resources are
   * considered to be released, even though further operations on the component can typically be
   * done. Therefore, this method should only contain logic that is necessary for a safe and
   * complete cleanup.
   * </p>
   */
  protected void onDidDestroy() {
    // pass
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
