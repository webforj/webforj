package com.webforj;

import com.webforj.annotation.Experimental;
import com.webforj.component.Component;
import com.webforj.component.ComponentUtil;
import com.webforj.component.Composite;
import com.webforj.concern.HasStyle;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.UUID;
import java.util.function.Consumer;

/**
 * Builder for creating animated transitions between views using the View Transition API.
 *
 * <p>
 * View transitions provide smooth animations when components are added, removed, or updated. This
 * class coordinates between the server-side component changes and the client-side View Transition
 * API handling.
 * </p>
 *
 * <h2>Basic Usage</h2>
 * <p>
 * Simple transition when swapping views:
 * </p>
 *
 * <pre>
 * {@code
 * Page.getCurrent().startViewTransition().onUpdate(done -> {
 *   container.remove(oldView);
 *   container.add(newView);
 *   done.run(); // Signal that DOM updates are complete
 * }).start();
 * }
 * </pre>
 *
 * <h2>Shared Element Transitions (Morphing)</h2>
 * <p>
 * For smooth morphing animations between views, mark elements with matching transition names using
 * {@link HasStyle#setViewTransitionName(String)}. Elements with the same name are automatically
 * paired and animated by the browser.
 * </p>
 *
 * <pre>
 * // In list view
 * avatar.setViewTransitionName("profile-avatar-" + id);
 * name.setViewTransitionName("profile-name-" + id);
 *
 * // In detail view - use same names for morphing
 * detailAvatar.setViewTransitionName("profile-avatar-" + id);
 * detailName.setViewTransitionName("profile-name-" + id);
 *
 * // Navigate - elements with matching names morph automatically
 * Page.getCurrent().startViewTransition().onUpdate(done -&gt; {
 *   Router.getCurrent().navigate(DetailView.class, params);
 *   done.run();
 * }).start();
 * </pre>
 *
 * <h2>Exit and Enter Animations</h2>
 * <p>
 * Use {@link #exit(Component, String)} and {@link #enter(Component, String)} to apply specific
 * animation types to components. The animation type sets the {@code view-transition-name} CSS
 * property which is used by CSS to target the transition pseudo-elements.
 * </p>
 *
 * <p>
 * ⚠️ WARNING: This class is experimental since 25.11 and may change in future releases.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 25.11
 */
@Experimental(since = "25.11")
public final class ViewTransition {

  /**
   * No transition - elements appear/disappear instantly without animation.
   */
  public static final String NONE = "vt-none";

  /**
   * Fade transition type - elements fade in and out.
   */
  public static final String FADE = "vt-fade";

  /**
   * Slide left transition type - old element slides out to the left, new element slides in from the
   * right.
   */
  public static final String SLIDE_LEFT = "vt-slide-left";

  /**
   * Slide right transition type - old element slides out to the right, new element slides in from
   * the left.
   */
  public static final String SLIDE_RIGHT = "vt-slide-right";

  /**
   * Slide up transition type - old element slides out upward, new element slides in from below.
   */
  public static final String SLIDE_UP = "vt-slide-up";

  /**
   * Slide down transition type - old element slides out downward, new element slides in from above.
   */
  public static final String SLIDE_DOWN = "vt-slide-down";

  /**
   * Zoom transition type - old element zooms out (shrinks), new element zooms in (grows).
   */
  public static final String ZOOM = "vt-zoom";

  /**
   * Zoom out transition type - old element zooms in (grows), new element zooms out (shrinks). This
   * is the reverse of the standard zoom effect.
   */
  public static final String ZOOM_OUT = "vt-zoom-out";

  private final Page page;
  private final ViewTransitionManager manager;
  private final String transitionId;
  private Consumer<Runnable> updateCallback;
  private Runnable readyCallback;
  private String defaultType;
  private final Map<Component, String> exitComponents = new LinkedHashMap<>();
  private final Map<Component, String> enterComponents = new LinkedHashMap<>();

  /**
   * Creates a new ViewTransition builder.
   *
   * @param page the page instance to execute the transition on
   * @param manager the transition manager for coordination
   */
  ViewTransition(Page page, ViewTransitionManager manager) {
    this.page = page;
    this.manager = manager;
    this.transitionId = UUID.randomUUID().toString();
  }

  /**
   * Sets the callback to execute during the transition's update phase.
   *
   * <p>
   * This callback is invoked after the "old" state is captured and before the "new" state is
   * captured. All component changes (adding/removing components, navigation) should happen here.
   * </p>
   *
   * <p>
   * The callback receives a "done" runnable that MUST be called when all DOM mutations and style
   * changes are complete. This allows the view transition to capture the new state at the right
   * time.
   * </p>
   *
   * <p>
   * For shared element transitions, elements with matching {@code view-transition-name} styles are
   * automatically paired and animated.
   * </p>
   *
   * @param callback the callback that receives a done handler to call when updates are complete
   * @return this builder for chaining
   */
  public ViewTransition onUpdate(Consumer<Runnable> callback) {
    this.updateCallback = callback;
    return this;
  }

  /**
   * Sets a callback to execute when the transition is ready to animate.
   *
   * <p>
   * This callback is invoked after both old and new states are captured and the animation is about
   * to begin. Useful for triggering additional effects synchronized with the transition.
   * </p>
   *
   * @param callback the callback to execute when ready
   * @return this builder for chaining
   */
  public ViewTransition onReady(Runnable callback) {
    this.readyCallback = callback;
    return this;
  }

  /**
   * Sets the default transition type for CSS targeting.
   *
   * <p>
   * This type is used as a fallback when {@link #exit(Component)} or {@link #enter(Component)} are
   * called without specifying a type. The type is applied as the {@code view-transition-name} CSS
   * property to enable CSS targeting like:
   * </p>
   *
   * <pre>
   * {@code
   * ::view-transition-old(custom-fade-effect) {
   *   animation: fade-out 300ms;
   * }
   * ::view-transition-new(custom-fade-effect) {
   *   animation: fade-in 300ms;
   * }
   * }
   * </pre>
   *
   * @param type the default transition type name
   * @return this builder for chaining
   */
  public ViewTransition type(String type) {
    this.defaultType = type;
    return this;
  }

  /**
   * Marks a component as an exit boundary with the default animation type.
   *
   * @param component the component being removed
   * @return this builder for chaining
   * @see #exit(Component, String)
   */
  public ViewTransition exit(Component component) {
    return exit(component, null);
  }

  /**
   * Marks a component as an exit boundary with a specific animation type.
   *
   * <p>
   * The animation type sets the {@code view-transition-name} CSS property on the component,
   * enabling CSS to target the transition pseudo-elements.
   * </p>
   *
   * <p>
   * Example:
   * </p>
   *
   * <pre>
   * {@code
   * page.startViewTransition().exit(oldView, ViewTransition.SLIDE_LEFT).onUpdate(done -> {
   *   container.remove(oldView);
   *   container.add(newView);
   *   done.run();
   * }).start();
   * }
   * </pre>
   *
   * @param component the component being removed
   * @param animationType the animation type (e.g., {@link #FADE}, {@link #SLIDE_LEFT})
   * @return this builder for chaining
   */
  public ViewTransition exit(Component component, String animationType) {
    exitComponents.put(component, animationType);
    // unlike enter, we should not apply style here - it will be applied when "prepare" event
    // arrives from client This ensures the style is in the DOM before startViewTransition captures
    // old state
    return this;
  }

  /**
   * Marks a component as an enter boundary with the default animation type.
   *
   * @param component the component being added
   * @return this builder for chaining
   * @see #enter(Component, String)
   */
  public ViewTransition enter(Component component) {
    return enter(component, null);
  }

  /**
   * Marks a component as an enter boundary with a specific animation type.
   *
   * <p>
   * Example:
   * </p>
   *
   * <pre>
   * {@code
   * page.startViewTransition().exit(oldView, ViewTransition.FADE)
   *     .enter(newView, ViewTransition.SLIDE_LEFT).onUpdate(() -> {
   *       container.remove(oldView);
   *       container.add(newView);
   *     }).start();
   * }
   * </pre>
   *
   * @param component the component being added
   * @param animationType the animation type (e.g., {@link #FADE}, {@link #SLIDE_LEFT})
   * @return this builder for chaining
   */
  public ViewTransition enter(Component component, String animationType) {
    enterComponents.put(component, animationType);
    applyTransitionStyles(component, animationType, "enter");
    return this;
  }

  /**
   * Starts the view transition.
   *
   * <p>
   * This method initiates the view transition and executes the update callback.
   * </p>
   *
   * <p>
   * Elements with matching {@code view-transition-name} CSS properties are automatically paired for
   * morphing animations. Set these on components before starting the transition.
   * </p>
   *
   * <p>
   * If view transitions are not supported, the update callback is executed immediately.
   * </p>
   *
   * @throws IllegalStateException if no update callback has been set
   */
  public void start() {
    if (updateCallback == null) {
      throw new IllegalStateException("onUpdate callback must be set before starting transition");
    }

    // Register and start the transition
    page.executeJsVoidAsync(buildTransitionScript());
  }

  /**
   * Executes the update callback. Called by the JS coordination code.
   *
   * @param onComplete callback to invoke when the update is complete
   */
  void executeUpdate(Runnable onComplete) {
    if (updateCallback != null) {
      updateCallback.accept(onComplete);
    } else {
      onComplete.run();
    }
  }

  /**
   * Executes the ready callback. Called by the JS coordination code.
   */
  void executeReady() {
    if (readyCallback != null) {
      readyCallback.run();
    }
  }

  /**
   * Applies exit styles to exit components. Called when "prepare" event arrives from client. This
   * ensures styles are in DOM before startViewTransition captures old state.
   */
  void applyExitStyles() {
    for (Map.Entry<Component, String> entry : exitComponents.entrySet()) {
      applyTransitionStyles(entry.getKey(), entry.getValue(), "exit");
    }
  }

  /**
   * Gets the transition ID.
   *
   * @return the unique transition ID
   */
  String getTransitionId() {
    return transitionId;
  }

  /**
   * Gets the default transition type.
   *
   * @return the default transition type, or null if not set
   */
  String getDefaultType() {
    return defaultType;
  }

  private void applyTransitionStyles(Component component, String animationType, String suffix) {
    if (component == null || animationType == null || NONE.equals(animationType)) {
      return;
    }

    Component target =
        component instanceof Composite<?> ? ComponentUtil.getBoundComponent(component) : component;

    if (target instanceof HasStyle<?> hasStyle) {
      // Append suffix (exit/enter) to create unique names for old and new views
      // This prevents morphing and allows independent animations
      String transitionName = animationType + "-" + suffix;
      hasStyle.setStyle("view-transition-name", transitionName);
    }
  }

  private String buildTransitionScript() {
    // Register the transition with the manager
    manager.register(transitionId, this);

    // Call the startTransition function from the external JS file
    return "window.__webforjViewTransitions.startTransition('" + transitionId + "');";
  }
}
