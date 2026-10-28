package com.webforj.testbed.runtime.sysgui;

import com.basis.startup.type.BBjException;
import com.basis.startup.type.BBjVector;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * The mock backing the multi line text control. The text is one document, the paragraphs are its
 * lines.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public class BBjCEditMock extends BBjControlMock {
  private BBjVector selection = new BBjVector(List.of(0, 0, 0, 0));

  BBjCEditMock(int id) {
    super(id);
  }

  List<String> getParagraphs() {
    return paragraphs();
  }

  @Override
  protected Object handle(Object proxy, Method method, Object[] args) throws Exception {
    return switch (method.getName()) {
      case "addParagraph" -> {
        List<String> paragraphs = paragraphs();
        int index = (Integer) args[0];
        List<String> added = lines((String) args[1]);
        if (index < 0 || index >= paragraphs.size()) {
          paragraphs.addAll(added);
        } else {
          paragraphs.addAll(index, added);
        }
        store(paragraphs);
        yield null;
      }
      case "appendToParagraph" -> {
        List<String> paragraphs = paragraphs();
        int index = (Integer) args[0];
        if (index >= 0 && index < paragraphs.size()) {
          List<String> appended = lines(paragraphs.get(index) + args[1]);
          paragraphs.remove(index);
          paragraphs.addAll(index, appended);
          store(paragraphs);
        }
        yield null;
      }
      case "removeParagraph" -> {
        List<String> paragraphs = paragraphs();
        int index = (Integer) args[0];
        if (index < 0 || index >= paragraphs.size()) {
          throw new BBjException("Invalid index " + index, BBjException.ARG_ERR);
        }
        paragraphs.remove(index);
        store(paragraphs);
        yield null;
      }
      case "getAllParagraphs" -> new BBjVector(paragraphs());
      case "getNumberOfParagraphs" -> paragraphs().size();
      case "setMaxLength" -> {
        if ((Integer) args[0] < 0) {
          throw new BBjException("Invalid max length " + args[0], BBjException.ARG_ERR);
        }
        putState("maxLength", args[0]);
        yield null;
      }
      case "setLineCountLimit", "setMaxParagraphSize", "setLineWrap", "setHorizontalScrollable",
          "setVerticalScrollable", "setWrapStyleWord", "setOvertypeMode" -> null;
      case "highlight" -> {
        selection = new BBjVector(List.of(args[0], args[1], args[2], args[3]));
        yield null;
      }
      case "getSelection" -> selection;
      default -> super.handle(proxy, method, args);
    };
  }

  private List<String> paragraphs() {
    String text = (String) getState("text");
    if (text == null || text.isEmpty()) {
      return new ArrayList<>();
    }

    return lines(text);
  }

  private static List<String> lines(String text) {
    return new ArrayList<>(Arrays.asList(text.split("\\n", -1)));
  }

  private void store(List<String> paragraphs) {
    putState("text", String.join("\n", paragraphs));
  }
}
