package com.webforj.devtools.craftforj.inspector.contribution.utilities;

import com.webforj.devtools.craftforj.source.SourceModificationException;
import java.util.List;
import java.util.Map;

/** Source aliases of the framework's concrete text and label implementations. */
public final class TextSourceAliases {

  private TextSourceAliases() {}

  /**
   * Resolves aliases from the actual method implementation, retaining subclass overrides.
   *
   * @param componentType the component class
   * @param methodName the requested text or label setter
   * @return aliases that have the same effect as the requested setter
   */
  public static Map<String, List<String>> getAliases(Class<?> componentType, String methodName) {
    if (componentType == null) {
      throw new SourceModificationException(
          "Cannot determine source aliases for '" + methodName + "' without the component type");
    }
    try {
      String owner =
          componentType.getMethod(methodName, String.class).getDeclaringClass().getName();
      String alias = null;
      if ("setText".equals(methodName)) {
        alias = switch (owner) {
          case "com.webforj.component.badge.Badge", "com.webforj.component.avatar.Avatar",
              "com.webforj.component.accordion.AccordionPanel" ->
            "setLabel";
          case "com.webforj.component.alert.Alert", "com.webforj.component.loading.Loading",
              "com.webforj.component.toast.Toast",
              "com.webforj.component.infinitescroll.InfiniteScroll" ->
            "setHtml";
          case "com.webforj.component.markdown.MarkdownViewer" -> "setContent";
          default -> null;
        };
      } else if ("com.webforj.component.avatar.Avatar".equals(owner)) {
        alias = "setText";
      }

      return alias == null ? Map.of() : Map.of(alias, List.of(methodName));
    } catch (NoSuchMethodException e) {
      return Map.of();
    }
  }
}
