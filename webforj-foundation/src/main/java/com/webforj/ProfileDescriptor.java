package com.webforj;

import com.google.gson.Gson;
import com.google.gson.JsonObject;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Defines the application manifest.
 *
 * <p>
 * A manifest descriptor for the application.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 24.21
 */
public class ProfileDescriptor {

  /**
   * Enum for display modes.
   *
   * <p>
   * The display mode determines how much of the browser UI is shown to the user when the app is
   * launched within the context of an operating system. You can choose to show the full browser
   * interface or hide it to provide a more app-like experience.
   * </p>
   */
  public enum Display {
    /**
     * Opens the app with browser UI elements hidden and uses the entirety of the available display
     * area. Use this value for apps where fullscreen engagement is crucial and desired.
     */
    FULLSCREEN,

    /**
     * Opens the app to look and feel like a standalone native app. This can include the app having
     * a different window and its own icon in the app launcher.
     */
    STANDALONE,

    /**
     * Opens the app to look and feel like a standalone app but with a minimal set of UI elements
     * for navigation. The specific elements can vary by browser but typically include navigation
     * controls like back, forward, reload, and possibly a way to view the app's URL.
     */
    MINIMAL_UI,

    /**
     * Opens the app in a conventional browser tab or new window, using the platform-specific
     * convention for opening links. This is the default value if no display mode is specified.
     */
    BROWSER
  }

  /**
   * Enum for orientation modes.
   *
   * <p>
   * The orientation mode determines the default orientation for the app.
   * </p>
   */
  public enum Orientation {
    /**
     * Displays the app in any orientation allowed by the device's operating system or user
     * settings. It allows the app to rotate freely to match the orientation of the device when it
     * is rotated.
     */
    ANY,

    /**
     * Displays the app in the orientation considered most natural for the device, as determined by
     * the browser, operating system, user settings, or the screen itself. It corresponds to how the
     * device is most commonly held or used:
     * <ul>
     * <li>On devices typically held vertically, such as mobile phones, natural is usually
     * portrait-primary.</li>
     * <li>On devices typically used horizontally, such as computer monitors and tablets, natural is
     * usually landscape-primary.</li>
     * </ul>
     * When the device is rotated, the app may or may not rotate so as to match the device's natural
     * orientation; this behavior may vary depending on the specific device, browser implementation,
     * and user settings.
     */
    NATURAL,

    /**
     * Displays the app with height greater than width. It allows the app to switch between
     * portrait-primary and portrait-secondary orientations when the device is rotated.
     */
    PORTRAIT,

    /**
     * Displays the app in portrait mode, typically with the device held upright. This is usually
     * the default app orientation on devices that are naturally portrait. Depending on the device
     * and browser implementation, the app will typically maintain this orientation even when the
     * device is rotated.
     */
    PORTRAIT_PRIMARY,

    /**
     * Displays the app in inverted portrait mode, which is portrait-primary rotated 180 degrees.
     * Depending on the device and browser implementation, the app will typically maintain this
     * orientation even when the device is rotated.
     */
    PORTRAIT_SECONDARY,

    /**
     * Displays the app with width greater than height. It allows the app to switch between
     * landscape-primary and landscape-secondary orientations when the device is rotated.
     */
    LANDSCAPE,

    /**
     * Displays the app in landscape mode, typically with the device held in its standard horizontal
     * position. This is usually the default app orientation on devices that are naturally
     * landscape. Depending on the device and browser implementation, the app will typically
     * maintain this orientation even when the device is rotated.
     */
    LANDSCAPE_PRIMARY,

    /**
     * Displays the app in inverted landscape mode, which is landscape-primary rotated 180 degrees.
     * Depending on the device and browser implementation, the app will typically maintain this
     * orientation even when the device is rotated.
     */
    LANDSCAPE_SECONDARY
  }

  private String shortName;
  private final String name;
  private final String startUrl;
  private final Display display;
  private final String themeColor;
  private final String backgroundColor;
  private final String description;
  private final String base;
  private final Orientation orientation;
  private final List<String> categories;
  private final String id;
  private final List<Image> icons;
  private final List<Image> screenshots;

  private ProfileDescriptor(ProfileDescriptorBuilder builder) {
    this.shortName = builder.shortName;
    this.name = builder.name;
    this.startUrl = builder.startUrl;
    this.display = builder.display;
    this.themeColor = builder.themeColor;
    this.backgroundColor = builder.backgroundColor;
    this.description = builder.description;
    this.base = builder.base;
    this.orientation = builder.orientation;
    this.categories = builder.categories;
    this.id = builder.id != null ? builder.id : builder.base;
    this.icons = builder.icons;
    this.screenshots = builder.screenshots;
  }

  /**
   * Creates a new AppConfigurationBuilder instance.
   *
   * @return the AppConfigurationBuilder instance
   */
  public static ProfileDescriptorBuilder create() {
    return new ProfileDescriptorBuilder();
  }

  /**
   * Gets the base of the application.
   *
   * @return the base
   */
  public String getBase() {
    return base;
  }

  /**
   * Gets the short name of the application.
   *
   * <p>
   * The short name is used to specify a short name for the application, which may be used when the
   * full name is too long for the available space.
   * </p>
   *
   * @return the short name
   */
  public String getShortName() {
    return shortName;
  }

  /**
   * Gets the name of the application.
   *
   * <p>
   * The name is used to specify the full name of the application as it's usually displayed to
   * users, such as in application lists or as a label for your application's icon.
   * </p>
   *
   * @return the name
   */
  public String getName() {
    return name;
  }

  /**
   * Gets the description of the application.
   *
   * @return the description
   */
  public String getDescription() {
    return description;
  }

  /**
   * Gets the start URL of the application.
   *
   * <p>
   * The start url is used to specify the URL that should be opened when a user launches the
   * application, such as when tapping the application's icon on their device's home screen or in an
   * application list.
   * </p>
   *
   * @return the start URL
   */
  public String getStartUrl() {
    return startUrl;
  }

  /**
   * Gets the display mode of the application.
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
  public Display getDisplay() {
    return display;
  }

  /**
   * Gets the background color of the application.
   *
   * <p>
   * The background color is used to specify an initial background color for the application. This
   * color appears in the application window before your application's stylesheets have loaded.
   * </p>
   *
   * @return the background color
   */
  public String getBackgroundColor() {
    return backgroundColor;
  }

  /**
   * Gets the theme color of the application.
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
  public String getThemeColor() {
    return themeColor;
  }

  /**
   * Gets the orientation of the application.
   *
   * @return the orientation
   */
  public Orientation getOrientation() {
    return orientation;
  }

  /**
   * Gets the categories of the application.
   *
   * @return the categories
   */
  public List<String> getCategories() {
    return categories;
  }

  /**
   * Gets the id of the application.
   *
   * @return the id
   */
  public String getId() {
    return id;
  }

  /**
   * Gets the icons of the application.
   *
   * @return the icons
   */
  public List<Image> getIcons() {
    return icons;
  }

  /**
   * Gets the screenshots of the application.
   *
   * @return the screenshots
   */
  public List<Image> getScreenshots() {
    return screenshots;
  }

  /**
   * Converts the ManifestDescriptor to a JsonObject.
   *
   * @return the JsonObject representation of the manifest
   */
  public JsonObject toJson() {
    Map<String, Object> manifestMap = new HashMap<>();
    manifestMap.put("short_name", getShortName());
    manifestMap.put("name", getName());
    manifestMap.put("description", getDescription());
    manifestMap.put("start_url", getStartUrl());
    manifestMap.put("display", getDisplay() != null ? getDisplay().name().toLowerCase() : null);
    manifestMap.put("theme_color", getThemeColor());
    manifestMap.put("background_color", getBackgroundColor());
    manifestMap.put("orientation",
        getOrientation() != null ? getOrientation().name().toLowerCase() : null);
    manifestMap.put("categories", getCategories());
    manifestMap.put("id", getId());

    // Add icons
    List<Map<String, String>> iconsList = new ArrayList<>();
    for (Image icon : getIcons()) {
      Map<String, String> iconMap = new HashMap<>();
      iconMap.put("src", icon.getSrc());
      if (icon.getSizes() != null && !icon.getSizes().isEmpty()) {
        iconMap.put("sizes", icon.getSizes());
      }

      if (icon.getType() != null && !icon.getType().isEmpty()) {
        iconMap.put("type", icon.getType());
      }

      if (icon.getPurpose() != null && !icon.getPurpose().isEmpty()) {
        iconMap.put("purpose", icon.getPurpose());
      }
      iconsList.add(iconMap);
    }
    manifestMap.put("icons", iconsList);

    // Add screenshots
    List<Map<String, String>> screenshotsList = new ArrayList<>();
    for (Image screenshot : getScreenshots()) {
      Map<String, String> screenshotMap = new HashMap<>();
      screenshotMap.put("src", screenshot.getSrc());
      if (screenshot.getSizes() != null && !screenshot.getSizes().isEmpty()) {
        screenshotMap.put("sizes", screenshot.getSizes());
      }

      if (screenshot.getType() != null && !screenshot.getType().isEmpty()) {
        screenshotMap.put("type", screenshot.getType());
      }

      if (screenshot.getLabel() != null && !screenshot.getLabel().isEmpty()) {
        screenshotMap.put("label", screenshot.getLabel());
      }

      if (screenshot.getFormFactor() != null && !screenshot.getFormFactor().isEmpty()) {
        screenshotMap.put("form_factor", screenshot.getFormFactor());
      }

      if (screenshot.getPlatform() != null && !screenshot.getPlatform().isEmpty()) {
        screenshotMap.put("platform", screenshot.getPlatform());
      }

      screenshotsList.add(screenshotMap);
    }

    manifestMap.put("screenshots", screenshotsList);

    Gson gson = new Gson();
    return gson.toJsonTree(manifestMap).getAsJsonObject();
  }

  /**
   * Converts the ManifestDescriptor to a JSON string.
   *
   * @return the JSON string representation of the manifest
   */
  @Override
  public String toString() {
    return toJson().toString();
  }

  /**
   * Builder class for AppConfiguration.
   */
  public static final class ProfileDescriptorBuilder {
    private String base;
    private String shortName;
    private String name;
    private String description;
    private String startUrl;
    private Display display;
    private String themeColor;
    private String backgroundColor;
    private Orientation orientation;
    private List<String> categories;
    private String id;
    private List<Image> icons = new ArrayList<>();
    private List<Image> screenshots = new ArrayList<>();

    /**
     * Sets the id of the application.
     *
     * <p>
     * The id is used to specify the unique identifier for the web app.
     * </p>
     *
     * @param id the id
     * @return the builder instance
     */
    public ProfileDescriptorBuilder setId(String id) {
      this.id = id;
      return this;
    }

    /**
     * Sets the base of the application.
     *
     * @param base the base
     * @return the builder instance
     */
    public ProfileDescriptorBuilder setBase(String base) {
      this.base = base;
      return this;
    }

    /**
     * Sets the short name of the application.
     *
     * <p>
     * The short name is used to specify a short name for the application, which may be used when
     * the full name is too long for the available space.
     * </p>
     *
     * @param shortName the short name
     * @return the builder instance
     */
    public ProfileDescriptorBuilder setShortName(String shortName) {
      this.shortName = shortName;
      return this;
    }

    /**
     * Sets the name of the application.
     *
     * <p>
     * The name is used to specify the full name of the application as it's usually displayed to
     * users, such as in application lists or as a label for your application's icon.
     * </p>
     *
     * @param name the name
     * @return the builder instance
     */
    public ProfileDescriptorBuilder setName(String name) {
      this.name = name;
      return this;
    }

    /**
     * Sets the description of the application.
     *
     * @param description the description
     * @return the builder instance
     */
    public ProfileDescriptorBuilder setDescription(String description) {
      this.description = description;
      return this;
    }

    /**
     * Sets the start URL of the application.
     *
     * <p>
     * The start url is used to specify the URL that should be opened when a user launches the
     * application, such as when tapping the application's icon on their device's home screen or in
     * an application list.
     * </p>
     *
     * @param startUrl the start URL
     * @return the builder instance
     */
    public ProfileDescriptorBuilder setStartUrl(String startUrl) {
      String url = startUrl;
      if (url.equals(".")) {
        url = base;
      } else if (!url.startsWith("http")) {
        url = base + url;
      }

      this.startUrl = url;
      return this;
    }

    /**
     * Sets the background color of the application.
     *
     * <p>
     * The background color is used to specify an initial background color for the application. This
     * color appears in the application window before your application's stylesheets have loaded.
     * </p>
     *
     * @param backgroundColor the background color
     * @return the builder instance
     */
    public ProfileDescriptorBuilder setBackgroundColor(String backgroundColor) {
      this.backgroundColor = backgroundColor;
      return this;
    }

    /**
     * Sets the theme color of the application.
     *
     * <p>
     * The theme color member is used to specify the default color for the application's user
     * interface. This color may be applied to various browser UI elements, such as the toolbar,
     * address bar, and status bar. It can be particularly noticeable in contexts like the task
     * switcher or when the app is added to the home screen.
     * </p>
     *
     * @param themeColor the theme color
     * @return the builder instance
     */
    public ProfileDescriptorBuilder setThemeColor(String themeColor) {
      this.themeColor = themeColor;
      return this;
    }

    /**
     * Sets the display mode of the application.
     *
     * <p>
     * The display is used to specify your preferred display mode for the application. The display
     * mode determines how much of the browser UI is shown to the user when the app is launched
     * within the context of an operating system. You can choose to show the full browser interface
     * or hide it to provide a more app-like experience.
     * </p>
     *
     * @param display the display mode
     * @return the builder instance
     */
    public ProfileDescriptorBuilder setDisplay(Display display) {
      this.display = display;
      return this;
    }

    /**
     * Sets the orientation of the application.
     *
     * <p>
     * The orientation is used to specify the default orientation for the app.
     * </p>
     *
     * @param orientation the orientation
     * @return the builder instance
     */
    public ProfileDescriptorBuilder setOrientation(Orientation orientation) {
      this.orientation = orientation;
      return this;
    }

    /**
     * Sets the categories of the application.
     *
     * <p>
     * The categories are used to specify the categories for the web app.
     * </p>
     *
     * @param categories the categories
     * @return the builder instance
     */
    public ProfileDescriptorBuilder setCategories(List<String> categories) {
      this.categories = categories;
      return this;
    }

    /**
     * Adds an icon to the application.
     *
     * @param icon the icon
     * @return the builder instance
     */
    public ProfileDescriptorBuilder addIcon(Image icon) {
      this.icons.add(icon);
      return this;
    }

    /**
     * Adds a screenshot to the application.
     *
     * @param screenshot the screenshot
     * @return the builder instance
     */
    public ProfileDescriptorBuilder addScreenshot(Image screenshot) {
      this.screenshots.add(screenshot);
      return this;
    }

    /**
     * Builds the AppConfiguration instance.
     *
     * @return the AppConfiguration instance
     */
    public ProfileDescriptor build() {
      return new ProfileDescriptor(this);
    }
  }

  /**
   * Class representing an image (icon or screenshot).
   */
  public static class Image {
    private final String src;
    private final String sizes;
    private final String type;
    private final String purpose;
    private final String label;
    private final String formFactor;
    private final String platform;

    private Image(ImageBuilder builder) {
      this.src = builder.src;
      this.sizes = builder.sizes;
      this.type = builder.type;
      this.purpose = builder.purpose;
      this.label = builder.label;
      this.formFactor = builder.formFactor;
      this.platform = builder.platform;
    }

    /**
     * Gets the source of the image.
     *
     * @return the source
     */
    public String getSrc() {
      return src;
    }

    /**
     * Gets the sizes of the image.
     *
     * @return the sizes
     */
    public String getSizes() {
      return sizes;
    }

    /**
     * Gets the type of the image.
     *
     * @return the type
     */
    public String getType() {
      return type;
    }

    /**
     * Gets the purpose of the image.
     *
     * @return the purpose
     */
    public String getPurpose() {
      return purpose;
    }

    /**
     * Gets the label of the image.
     *
     * @return the label
     */
    public String getLabel() {
      return label;
    }

    /**
     * Gets the form factor of the image.
     *
     * @return the form factor
     */
    public String getFormFactor() {
      return formFactor;
    }

    /**
     * Gets the platform of the image.
     *
     * @return the platform
     */
    public String getPlatform() {
      return platform;
    }

    /**
     * Builder class for Image.
     */
    public static class ImageBuilder {
      private String src;
      private String sizes;
      private String type;
      private String purpose;
      private String label;
      private String formFactor;
      private String platform;

      /**
       * Sets the source of the image.
       *
       * @param src the source
       * @return the builder instance
       */
      public ImageBuilder setSrc(String src) {
        this.src = src;

        if (this.type == null || this.type.isEmpty()) {
          // support detection for SVG, png, jpg, jpeg, webp
          String ext = Paths.get(src).getFileName().toString().toLowerCase();
          if (ext.endsWith(".svg")) {
            this.type = "image/svg+xml";
          } else if (ext.endsWith(".png")) {
            this.type = "image/png";
          } else if (ext.endsWith(".jpg") || ext.endsWith(".jpeg")) {
            this.type = "image/jpeg";
          } else if (ext.endsWith(".webp")) {
            this.type = "image/webp";
          }
        }

        return this;
      }

      /**
       * Sets the sizes of the image.
       *
       * @param sizes the sizes
       * @return the builder instance
       */
      public ImageBuilder setSizes(String sizes) {
        this.sizes = sizes;
        return this;
      }

      /**
       * Sets the type of the image.
       *
       * @param type the type
       * @return the builder instance
       */
      public ImageBuilder setType(String type) {
        this.type = type;
        return this;
      }

      /**
       * Sets the purpose of the image.
       *
       * @param purpose the purpose
       * @return the builder instance
       */
      public ImageBuilder setPurpose(String purpose) {
        this.purpose = purpose;
        return this;
      }

      /**
       * Sets the label of the image.
       *
       * @param label the label
       * @return the builder instance
       */
      public ImageBuilder setLabel(String label) {
        this.label = label;
        return this;
      }

      /**
       * Sets the form factor of the image.
       *
       * @param formFactor the form factor
       * @return the builder instance
       */
      public ImageBuilder setFormFactor(String formFactor) {
        this.formFactor = formFactor;
        return this;
      }

      /**
       * Sets the platform of the image.
       *
       * @param platform the platform
       * @return the builder instance
       */
      public ImageBuilder setPlatform(String platform) {
        this.platform = platform;
        return this;
      }

      /**
       * Builds the Image instance.
       *
       * @return the Image instance
       */
      public Image build() {
        return new Image(this);
      }
    }
  }
}
