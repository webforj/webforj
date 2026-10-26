package com.webforj.automation.see;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.webforj.component.Component;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

/**
 * Formats a snapshot as text. Each component is an entry of a YAML list with its type, its text in
 * quotes and its attributes in brackets.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
final class SnapshotFormat {
  static final String ELLIPSIS = "…";
  private static final String INDENT = "  ";
  private static final Pattern BARE_VALUE = Pattern.compile("[A-Za-z0-9_.+/-]+");
  private static final Gson JSON = new GsonBuilder().disableHtmlEscaping().create();

  private SnapshotFormat() {}

  static String renderTree(Component root, SnapshotOptions options) {
    SnapshotRenderer renderer =
        new SnapshotRenderer(SnapshotSerializerRegistry.getCurrent(), options);
    if (!renderer.isShown(root)) {
      return "";
    }

    StringBuilder output = new StringBuilder();
    write(renderer.describe(root), output, 0, options.getMaxWidth());

    return output.toString();
  }

  private static void write(SnapshotNode node, StringBuilder output, int depth, int maxWidth) {
    List<SnapshotNode> items = node.getItems();
    Integer itemCount = node.getItemCount();
    if (itemCount != null && itemCount < items.size()) {
      throw new IllegalStateException("The snapshot of " + node.getType() + " holds " + items.size()
          + " items but states a count of " + itemCount + ".");
    }

    List<SnapshotNode> below = new ArrayList<>(node.getChildren());
    below.addAll(node.getStructure());
    below.addAll(items.subList(0, Math.min(items.size(), maxWidth)));

    output.append(INDENT.repeat(depth)).append("- ").append(scalar(lineOf(node), !below.isEmpty()));
    if (!below.isEmpty()) {
      output.append(':');
    }

    output.append('\n');
    for (SnapshotNode child : below) {
      write(child, output, depth + 1, maxWidth);
    }

    if (items.size() > maxWidth) {
      output.append(INDENT.repeat(depth + 1)).append("- ").append(ELLIPSIS);
      if (itemCount != null) {
        output.append(' ').append(itemCount - maxWidth).append(" more");
      }

      output.append('\n');
    }
  }

  private static String lineOf(SnapshotNode node) {
    StringBuilder line = new StringBuilder(node.getType());
    if (node.getText() != null) {
      line.append(' ').append(JSON.toJson(node.getText()));
    }

    Map<String, String> attributes = node.getAttributes();
    List<String> keys = new ArrayList<>();
    attributes.forEach((key, value) -> {
      if (value != null) {
        keys.add(key);
      }
    });
    attributes.forEach((key, value) -> {
      if (value == null) {
        keys.add(key);
      }
    });
    for (String key : keys) {
      String value = attributes.get(key);
      if (node.isTextAttribute(key) && value != null && value.equals(node.getText())) {
        continue;
      }

      line.append(" [").append(formatAttributeText(key));
      if (value != null) {
        line.append('=').append(formatAttributeText(value));
      }

      line.append(']');
    }

    return line.toString();
  }

  private static String formatAttributeText(String text) {
    return BARE_VALUE.matcher(text).matches() ? text : JSON.toJson(text);
  }

  private static String scalar(String line, boolean container) {
    boolean plain = !line.contains(": ") && !line.contains(" #") && !line.endsWith(":")
        && (container || !line.endsWith(" "));
    if (plain) {
      return line;
    }

    return "\"" + line.replace("\\", "\\\\").replace("\"", "\\\"") + "\"";
  }
}
