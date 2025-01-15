package com.webforj.annotation;

import com.basis.util.common.ServerConstants;
import com.webforj.App;
import com.webforj.Page;
import com.webforj.ProfileDescriptor;
import com.webforj.ProfileDescriptor.ProfileDescriptorBuilder;
import com.webforj.Request;
import com.webforj.component.Component;
import com.webforj.environment.ObjectTable;
import com.webforj.environment.StringTable;
import com.webforj.exceptions.WebforjRuntimeException;
import com.webforj.utilities.Assets;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.HashMap;

/**
 * Annotation processor for the application and controls annotations.
 *
 * @author Hyyan Abo Fakher
 */
public final class AnnotationProcessor {

  Page getPage() {
    return Page.getCurrent();
  }

  /**
   * Process the annotations of the application.
   *
   * @param app The application to process
   */
  public void processAppAnnotations(App app) {
    processConfiguration(app);
    processAppAttribute(app);
    processAppMeta(app);
    processLink(app);
    processStyleSheet(app);
    processInlineStyleSheet(app);
    processJavaScript(app);
    processInlineJavaScript(app);
    processAppDarkTheme(app);
    processAppLightTheme(app);
    processAppTheme(app);
    processAppProfile(app);
    processAppTitle(app);
  }

  /**
   * Process the annotations of the control.
   *
   * @param control The control to process
   */
  public void processControlAnnotations(Component control) {
    processConfiguration(control);
    processLink(control);
    processStyleSheet(control);
    processInlineStyleSheet(control);
    processJavaScript(control);
    processInlineJavaScript(control);
  }

  /**
   * Process the AppTitle annotation.
   *
   * @param clazz The class to process
   */
  private void processAppTitle(Object clazz) {
    AppTitle appTitle = clazz.getClass().getAnnotation(AppTitle.class);
    if (appTitle != null) {
      getPage().setTitle(appTitle.value(), appTitle.format());
    }
  }

  /**
   * Process the AppAttribute annotation.
   *
   * @param clazz The class to process
   */
  private void processAppAttribute(Object clazz) {
    AppAttribute[] appAttributes = clazz.getClass().getAnnotationsByType(AppAttribute.class);
    if (appAttributes != null) {
      for (AppAttribute appAttribute : appAttributes) {
        getPage().setAttribute(appAttribute.name(), appAttribute.value(), appAttribute.selector());
      }
    }
  }

  /**
   * Process the AppDarkTheme annotation.
   *
   * @param clazz The class to process
   */
  private void processAppDarkTheme(Object clazz) {
    AppDarkTheme appDarkTheme = clazz.getClass().getAnnotation(AppDarkTheme.class);
    if (appDarkTheme != null) {
      App.setDarkTheme(appDarkTheme.value());
    }
  }

  /**
   * Process the AppLightTheme annotation.
   *
   * @param clazz The class to process
   */
  private void processAppLightTheme(Object clazz) {
    AppLightTheme appLightTheme = clazz.getClass().getAnnotation(AppLightTheme.class);
    if (appLightTheme != null) {
      App.setLightTheme(appLightTheme.value());
    }
  }

  /**
   * Process the AppTheme annotation.
   *
   * @param clazz The class to process
   */
  private void processAppTheme(Object clazz) {
    AppTheme appTheme = clazz.getClass().getAnnotation(AppTheme.class);
    if (appTheme != null) {
      App.setTheme(appTheme.value());
    }
  }

  /**
   * Process the AppMeta annotation.
   *
   * @param clazz The class to process
   */
  private void processAppMeta(Object clazz) {
    AppMeta[] appMeta = clazz.getClass().getAnnotationsByType(AppMeta.class);
    if (appMeta != null) {
      for (AppMeta meta : appMeta) {
        HashMap<String, String> attributes = new HashMap<>();
        for (Attribute attribute : meta.attributes()) {
          attributes.put(attribute.name(), attribute.value());
        }

        getPage().setMeta(meta.name(), meta.content(), attributes);
      }
    }
  }

  /**
   * Process the StyleSheet annotation.
   *
   * @param clazz The class to process
   */
  private void processStyleSheet(Object clazz) {
    StyleSheet[] styleSheet = clazz.getClass().getAnnotationsByType(StyleSheet.class);
    if (styleSheet != null) {
      for (StyleSheet sheet : styleSheet) {
        HashMap<String, String> attributes = new HashMap<>();
        for (Attribute attribute : sheet.attributes()) {
          attributes.put(attribute.name(), attribute.value());
        }

        String key = "com.webforj.annotations.AnnotationProcessor::styles::" + sheet.value();
        if (sheet.top()) {
          key += "::top";
        }

        if (ObjectTable.contains(key)) {
          continue;
        }

        ObjectTable.put(key, true);
        getPage().addStyleSheet(sheet.value(), sheet.top(), attributes);
      }
    }
  }

  /**
   * Process the InlineStyleSheet annotation.
   *
   * @param clazz The class to process
   */
  private void processInlineStyleSheet(Object clazz) {
    InlineStyleSheet[] inlineStyleSheets =
        clazz.getClass().getAnnotationsByType(InlineStyleSheet.class);
    if (inlineStyleSheets != null) {
      for (InlineStyleSheet sheet : inlineStyleSheets) {
        HashMap<String, String> attributes = new HashMap<>();
        for (Attribute attribute : sheet.attributes()) {
          attributes.put(attribute.name(), attribute.value());
        }

        boolean hasId = sheet.id() != null && !sheet.id().isEmpty();
        String key = "com.webforj.annotations.AnnotationProcessor::inlineStyles::" + sheet.id();
        boolean isTracked = ObjectTable.contains(key);

        if (hasId) {
          if (isTracked) {
            continue;
          }

          attributes.put("id", sheet.id());

          if (sheet.once()) {
            ObjectTable.put(key, true);
          }
        }

        getPage().addInlineStyleSheet(sheet.value(), sheet.top(), attributes);
      }
    }
  }

  /**
   * Process the JavaScript annotation.
   *
   * @param clazz The class to process
   */
  private void processJavaScript(Object clazz) {
    JavaScript[] javaScript = clazz.getClass().getAnnotationsByType(JavaScript.class);
    if (javaScript != null) {
      for (JavaScript script : javaScript) {
        HashMap<String, String> attributes = new HashMap<>();
        for (Attribute attribute : script.attributes()) {
          attributes.put(attribute.name(), attribute.value());
        }

        String key = "com.webforj.annotations.AnnotationProcessor::scripts::" + script.value();
        if (script.top()) {
          key += "::top";
        }

        if (ObjectTable.contains(key)) {
          continue;
        }

        ObjectTable.put(key, true);
        getPage().addJavaScript(script.value(), script.top(), attributes);
      }
    }
  }

  /**
   * Process the InlineJavaScript annotation.
   *
   * @param clazz The class to process
   */
  private void processInlineJavaScript(Object clazz) {
    InlineJavaScript[] inlineJavascript =
        clazz.getClass().getAnnotationsByType(InlineJavaScript.class);
    if (inlineJavascript != null) {
      for (InlineJavaScript script : inlineJavascript) {
        HashMap<String, String> attributes = new HashMap<>();
        for (Attribute attribute : script.attributes()) {
          attributes.put(attribute.name(), attribute.value());
        }

        boolean hasId = script.id() != null && !script.id().isEmpty();
        if (hasId) {
          String key = "com.webforj.annotations.AnnotationProcessor::inlineScripts::" + script.id();
          boolean isTracked = ObjectTable.contains(key);

          if (isTracked) {
            continue;
          }

          attributes.put("id", script.id());
          ObjectTable.put(key, true);
        }

        getPage().addInlineJavaScript(script.value(), script.top(), attributes);
      }
    }
  }

  /**
   * Process the Link annotation.
   *
   * @param clazz The class to process
   */
  private void processLink(Object clazz) {
    Link[] links = clazz.getClass().getAnnotationsByType(Link.class);
    if (links != null) {
      for (Link link : links) {
        HashMap<String, String> attributes = new HashMap<>();
        for (Attribute attribute : link.attributes()) {
          attributes.put(attribute.name(), attribute.value());
        }

        String id = link.id() != null && !link.id().isEmpty() ? link.id() : link.value();
        String key = "com.webforj.annotations.AnnotationProcessor::links::" + id;
        if (link.top()) {
          key += "::top";
        }

        if (ObjectTable.contains(key)) {
          continue;
        }

        ObjectTable.put(key, true);
        getPage().addLink(link.value(), link.top(), attributes);
      }
    }
  }

  /**
   * Process the Configuration annotation.
   *
   * @param clazz The class to process
   */
  private void processConfiguration(Object clazz) {
    STConfiuration[] configurations = clazz.getClass().getAnnotationsByType(STConfiuration.class);
    if (configurations != null) {
      for (STConfiuration configuration : configurations) {
        String key = configuration.key();

        // make sure application level configurations are not cleared
        if (StringTable.contains(key)) {

          // is blacklisted ?
          String[] blackList = new String[] {"DEBUG", "PARSE_REQUEST_THEME"};
          if (Arrays.asList(blackList).contains(key)) {
            continue;
          }

          StringTable.clear(key);
        }

        StringTable.put(key, configuration.value());
      }
    }
  }

  /**
   * Process the AppProfile annotation.
   *
   * @param clazz The class to process
   */
  ProfileDescriptor processAppProfile(Object clazz) {
    AppProfile appProfile = clazz.getClass().getAnnotation(AppProfile.class);
    if (appProfile == null) {
      return null;
    }

    String base = getBaseUrl();
    ProfileDescriptorBuilder builder = initializeProfileDescriptorBuilder(appProfile, base);

    processDefaultIcon(appProfile, builder, base);
    processIcons(appProfile, builder, base);
    processScreenshots(appProfile, builder, base);

    ProfileDescriptor profileDescriptor = builder.build();
    setManifest(profileDescriptor);
    setMetaTags(appProfile, base);

    return profileDescriptor;
  }

  private String getBaseUrl() {
    try {
      return new URI(Request.getCurrent().getUrl()).resolve(".").toString();
    } catch (URISyntaxException e) {
      throw new WebforjRuntimeException(
          "Failed to process the AppProfile annotation. The base URL is invalid.", e);
    }
  }

  private ProfileDescriptorBuilder initializeProfileDescriptorBuilder(AppProfile appProfile,
      String base) {
    // @formatter:off
    return ProfileDescriptor.create()
        .setBase(base)
        .setId(appProfile.id().isEmpty() ? base : appProfile.id())
        .setShortName(appProfile.shortName())
        .setName(appProfile.name())
        .setDescription(appProfile.description())
        .setStartUrl(appProfile.startUrl())
        .setDisplay(appProfile.display())
        .setThemeColor(appProfile.themeColor())
        .setBackgroundColor(appProfile.backgroundColor())
        .setOrientation(appProfile.orientation())
        .setCategories(Arrays.asList(appProfile.categories()));
    // @formatter:on
  }

  private void processDefaultIcon(AppProfile appProfile, ProfileDescriptorBuilder builder,
      String base) {
    AppProfile.DefaultIcon defaultIcon = appProfile.defaultIcon();
    if (defaultIcon == null) {
      return;
    }

    String iconFilename = Paths.get(defaultIcon.value()).getFileName().toString();
    String iconBaseName = iconFilename.substring(0, iconFilename.lastIndexOf('.'));
    String iconExtension = iconFilename.substring(iconFilename.lastIndexOf('.'));

    for (int size : defaultIcon.sizes()) {
      String src = defaultIcon.value().replace(iconFilename,
          iconBaseName + "-" + size + "x" + size + iconExtension);
      // @formatter:off
      ProfileDescriptor.Image image = new ProfileDescriptor.Image.ImageBuilder()
          .setSrc(resolveUrl(src, base))
          .setSizes(size + "x" + size)
          .build();
      // @formatter:on

      builder.addIcon(image);
    }
  }

  private void processIcons(AppProfile appProfile, ProfileDescriptorBuilder builder, String base) {
    for (AppProfile.Icon icon : appProfile.icons()) {
      // @formatter:off
      ProfileDescriptor.Image.ImageBuilder imageBuilder = new ProfileDescriptor.Image.ImageBuilder()
          .setSrc(resolveUrl(icon.src(), base))
          .setSizes(icon.sizes())
          .setPurpose(icon.purpose());
      // @formatter:on

      // allow to detect the icon type based on the file extension
      if (!icon.type().isEmpty()) {
        imageBuilder.setType(icon.type());
      }

      builder.addIcon(imageBuilder.build());
    }
  }

  private void processScreenshots(AppProfile appProfile, ProfileDescriptorBuilder builder,
      String base) {
    for (AppProfile.Screenshot screenshot : appProfile.screenshots()) {
      // @formatter:off
      ProfileDescriptor.Image.ImageBuilder imageBuilder = new ProfileDescriptor.Image.ImageBuilder()
          .setSrc(resolveUrl(screenshot.src(), base))
          .setSizes(screenshot.sizes())
          .setLabel(screenshot.label())
          .setFormFactor(screenshot.formFactor())
          .setPlatform(screenshot.platform());
      // @formatter:on

      if (!screenshot.type().isEmpty()) {
        imageBuilder.setType(screenshot.type());
      }

      builder.addScreenshot(imageBuilder.build());
    }
  }

  private void setManifest(ProfileDescriptor profileDescriptor) {
    String registerManifest = "let element = document.createElement('link');"
        + "let encoded = encodeURIComponent(JSON.stringify(" + profileDescriptor.toString() + "));"
        + "element.setAttribute('rel', 'manifest');"
        + "element.setAttribute('href', 'data:application/manifest+json,' + encoded);"
        + "document.querySelector('head').appendChild(element);";

    getPage().executeJsVoidAsync(registerManifest);
  }

  private void setMetaTags(AppProfile appProfile, String base) {
    Page page = getPage();

    page.setMeta("msapplication-tap-highlight", "no");
    page.setMeta("apple-mobile-web-app-capable", "yes");
    page.setMeta("mobile-web-app-capable", "yes");
    page.setMeta("apple-touch-fullscreen", "yes");
    page.setMeta("msapplication-tap-highlight", "no");

    String name = appProfile.name();
    if (name != null && !name.isEmpty()) {
      page.setTitle(appProfile.name());
      page.setMeta("apple-mobile-web-app-title", appProfile.name());
    }

    String description = appProfile.description();
    if (description != null && !description.isEmpty()) {
      page.setMeta("description", description);
    }

    String viewport = appProfile.viewport();
    if (viewport != null && !viewport.isEmpty()) {
      page.setMeta("viewport", viewport);
    }

    String themeColor = appProfile.themeColor();
    if (themeColor != null && !themeColor.isEmpty()) {
      page.setMeta("theme-color", themeColor);
    }

    String backgroundColor = appProfile.backgroundColor();
    if (backgroundColor != null && !backgroundColor.isEmpty()) {
      page.setMeta("background-color", backgroundColor);
    }

    AppProfile.DefaultIcon defaultIcon = appProfile.defaultIcon();
    if (defaultIcon != null) {
      // @formatter:off
      ProfileDescriptor.Image image = new ProfileDescriptor.Image.ImageBuilder()
          .setSrc(resolveUrl(defaultIcon.value(), base))
          .build();
      // @formatter:on

      HashMap<String, String> attributes = new HashMap<>();
      attributes.put("rel", "shortcut icon");
      attributes.put("type", image.getType());
      page.addLink(image.getSrc(), true, attributes);
    }
  }

  private String resolveUrl(String src, String base) {
    String resolved = src;
    boolean isContext = Assets.isContextUrl(src);
    if (isContext) {
      throw new IllegalArgumentException(
          "The src attribute for App Icons and Screenshots must be a URL, not a context path.");
    }

    boolean isWs = Assets.isWebServerUrl(src);
    if (isWs) {
      resolved = Assets.resolveWebServerUrl(src);
    }

    // If resolved is not a fully qualified URL, resolve it with the base URL
    try {
      URI uri = new URI(resolved);
      if (!uri.isAbsolute()) {
        // If not absolute, combine with the base URL
        URI baseUri = new URI(base);
        resolved = baseUri.resolve(uri).toString();
      }
    } catch (URISyntaxException e) {
      throw new IllegalArgumentException("Invalid URL format: " + resolved, e);
    }

    return resolved;
  }
}
