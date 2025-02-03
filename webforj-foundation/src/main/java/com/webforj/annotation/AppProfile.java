package com.webforj.annotation;

import com.webforj.ProfileDescriptor;
import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Annotation to define the profile of an application.
 *
 * <p>
 * This annotation is used to specify various properties of an application, such as its name,
 * display mode, theme color, background color, start URL, and icon sizes. It helps in configuring
 * the application's manifest and how it should be presented to the user.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 24.21
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
@Inherited
public @interface AppProfile {
  String DEFAULT_THEME_COLOR = "#ffffff";
  String DEFAULT_BACKGROUND_COLOR = "#f8fafc";
  String DEFAULT_START_URL = ".";
  String DEFAULT_VIEWPORT =
      "viewport-fit=cover, width=device-width, initial-scale=1.0, minimum-scale=1.0, maximum-scale=1.0, user-scalable=no";
  String DEFAULT_ICON_SRC = "icons://icon.png";

  /**
   * The id of the application.
   *
   * <p>
   * The id is used to specify the unique identifier for the app.
   * </p>
   *
   * @return the id
   */
  String id() default "";

  /**
   * The short name of the application.
   *
   * <p>
   * The short name is used to specify a short name for the application, which may be used when the
   * full name is too long for the available space.
   * </p>
   *
   * @return the short name
   */
  String shortName();

  /**
   * The full name of the application.
   *
   * <p>
   * The name is used to specify the full name of the application as it's usually displayed to
   * users, such as in application lists or as a label for your application's icon.
   * </p>
   *
   * @return the name
   */
  String name();

  /**
   * The description of the application.
   *
   * @return the description
   */
  String description() default "";

  /**
   * Defines the default viewport for the application.
   *
   * @return the viewport
   */
  String viewport() default DEFAULT_VIEWPORT;

  /**
   * The display mode of the application.
   *
   * <p>
   * The display is used to specify your preferred display mode for the application. The display
   * mode determines how much of the browser UI is shown to the user when the app is launched within
   * the context of an operating system. You can choose to show the full browser interface or hide
   * it to provide a more app-like experience.
   * </p>
   *
   * @return the display mode
   */
  ProfileDescriptor.Display display() default ProfileDescriptor.Display.STANDALONE;

  /**
   * The theme color of the application.
   *
   * <p>
   * The theme color member is used to specify the default color for the application's user
   * interface. This color may be applied to various browser UI elements, such as the toolbar,
   * address bar, and status bar. It can be particularly noticeable in contexts like the task
   * switcher or when the app is added to the home screen.
   * </p>
   *
   * @return the theme color
   */
  String themeColor() default DEFAULT_THEME_COLOR;

  /**
   * The background color of the application.
   *
   * <p>
   * The background color is used to specify an initial background color for the application. This
   * color appears in the application window before your application's stylesheets have loaded.
   * </p>
   *
   * @return the background color
   */
  String backgroundColor() default DEFAULT_BACKGROUND_COLOR;

  /**
   * The start URL of the application.
   *
   * <p>
   * The start url is used to specify the URL that should be opened when a user launches the
   * application, such as when tapping the application's icon on their device's home screen or in an
   * application list.
   * </p>
   *
   * @return the start URL
   */
  String startUrl() default DEFAULT_START_URL;

  /**
   * The orientation of the application.
   *
   * <p>
   * The orientation is used to specify the default orientation for the app.
   * </p>
   *
   * @return the orientation
   */
  ProfileDescriptor.Orientation orientation() default ProfileDescriptor.Orientation.NATURAL;

  /**
   * The categories of the application.
   *
   * <p>
   * The categories are used to specify the categories for the app.
   * </p>
   *
   * @return the categories
   */
  String[] categories() default {};

  /**
   * The default icon of the application.
   *
   * <p>
   * The icon path can be be a <code>webserver://</code> or <code>icons://</code> URL.
   * </p>
   *
   * @return the default icon
   */
  DefaultIcon defaultIcon() default @DefaultIcon(DEFAULT_ICON_SRC);

  /**
   * The icons of the application.
   *
   * <p>
   * The icons are used to specify the icons for the app.
   * </p>
   *
   * @return the icons
   */
  Icon[] icons() default {};

  /**
   * The screenshots of the application.
   *
   * <p>
   * The screenshots are used to specify the screenshots for the app.
   * </p>
   *
   * @return the screenshots
   */
  Screenshot[] screenshots() default {};

  /**
   * Annotation to define a default icon for the application.
   *
   * <p>
   * This annotation is used to specify the default icon for the application. It helps in
   * configuring the application's default icon and how it should be presented to the user.
   * </p>
   */
  @interface DefaultIcon {
    /**
     * The source of the icon.
     *
     * @return the source
     */
    String value();

    /**
     * The available sizes of the icon.
     *
     * @return the sizes
     */
    int[] sizes() default {144, 192, 512};
  }

  /**
   * Annotation to define an icon for the application.
   *
   * <p>
   * This annotation is used to specify various properties of an icon, such as its source, sizes,
   * type, and purpose. It helps in configuring the application's icons and how they should be
   * presented to the user.
   * </p>
   */
  @interface Icon {
    /**
     * The source of the icon.
     *
     * @return the source
     */
    String src();

    /**
     * The sizes of the icon.
     *
     * @return the sizes
     */
    String sizes();

    /**
     * The type of the icon.
     *
     * @return the type
     */
    String type() default "";

    /**
     * The purpose of the icon.
     *
     * @return the purpose
     */
    String purpose() default "";
  }

  /**
   * Annotation to define a screenshot for the application.
   *
   * <p>
   * This annotation is used to specify various properties of a screenshot, such as its source,
   * sizes, type, label, form factor, and platform. It helps in configuring the application's
   * screenshots and how they should be presented to the user.
   * </p>
   */
  @interface Screenshot {
    /**
     * The source of the screenshot.
     *
     * @return the source
     */
    String src();

    /**
     * The sizes of the screenshot.
     *
     * @return the sizes
     */
    String sizes();

    /**
     * The type of the screenshot.
     *
     * @return the type
     */
    String type() default "";

    /**
     * The label of the screenshot.
     *
     * @return the label
     */
    String label() default "";

    /**
     * The form factor of the screenshot.
     *
     * @return the form factor
     */
    String formFactor() default "";

    /**
     * The platform of the screenshot.
     *
     * @return the platform
     */
    String platform() default "";
  }
}
