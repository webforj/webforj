package com.webforj;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import java.util.Arrays;
import java.util.List;
import org.junit.jupiter.api.Test;

class ProfileDescriptorTest {

  private static final String SHORT_NAME = "ShortName";
  private static final String FULL_NAME = "FullName";
  private static final String DESCRIPTION = "This is a test application";
  private static final String START_URL = "http://example.com";
  private static final String THEME_COLOR = "#FFFFFF";
  private static final String BACKGROUND_COLOR = "#000000";
  private static final String BASE_URL = "http://base.com";
  private static final String APP_ID = "appId";
  private static final List<String> CATEGORIES = Arrays.asList("category1", "category2");

  @Test
  void shouldCreateManifestDescriptorWithAllFields() {
    // @formatter:off
    ProfileDescriptor descriptor = ProfileDescriptor.create()
        .setShortName(SHORT_NAME)
        .setName(FULL_NAME)
        .setDescription(DESCRIPTION)
        .setStartUrl(START_URL)
        .setDisplay(ProfileDescriptor.Display.STANDALONE)
        .setThemeColor(THEME_COLOR)
        .setBackgroundColor(BACKGROUND_COLOR)
        .setBase(BASE_URL)
        .setOrientation(ProfileDescriptor.Orientation.LANDSCAPE)
        .setCategories(CATEGORIES)
        .setId(APP_ID)
        .addIcon(new ProfileDescriptor.Image.ImageBuilder().setSrc("icon.png").setSizes("48x48")
            .setType("image/png").setPurpose("any").build())
        .addScreenshot(new ProfileDescriptor.Image.ImageBuilder().setSrc("screenshot.png")
            .setSizes("1280x720").setType("image/png").setLabel("Screenshot Label")
            .setFormFactor("wide").setPlatform("android").build())
        .build();
    // @formatter:on

    assertEquals(SHORT_NAME, descriptor.getShortName());
    assertEquals(FULL_NAME, descriptor.getName());
    assertEquals(DESCRIPTION, descriptor.getDescription());
    assertEquals(START_URL, descriptor.getStartUrl());
    assertEquals(ProfileDescriptor.Display.STANDALONE, descriptor.getDisplay());
    assertEquals(THEME_COLOR, descriptor.getThemeColor());
    assertEquals(BACKGROUND_COLOR, descriptor.getBackgroundColor());
    assertEquals(BASE_URL, descriptor.getBase());
    assertEquals(ProfileDescriptor.Orientation.LANDSCAPE, descriptor.getOrientation());
    assertEquals(CATEGORIES, descriptor.getCategories());
    assertEquals(APP_ID, descriptor.getId());
    assertEquals(1, descriptor.getIcons().size());
    assertEquals("icon.png", descriptor.getIcons().get(0).getSrc());
    assertEquals("48x48", descriptor.getIcons().get(0).getSizes());
    assertEquals("image/png", descriptor.getIcons().get(0).getType());
    assertEquals("any", descriptor.getIcons().get(0).getPurpose());
    assertEquals(1, descriptor.getScreenshots().size());
    assertEquals("screenshot.png", descriptor.getScreenshots().get(0).getSrc());
    assertEquals("1280x720", descriptor.getScreenshots().get(0).getSizes());
    assertEquals("image/png", descriptor.getScreenshots().get(0).getType());
    assertEquals("Screenshot Label", descriptor.getScreenshots().get(0).getLabel());
    assertEquals("wide", descriptor.getScreenshots().get(0).getFormFactor());
    assertEquals("android", descriptor.getScreenshots().get(0).getPlatform());
  }

  @Test
  void shouldReturnDefaultValuesWhenNotSet() {
    // @formatter:off
    ProfileDescriptor descriptor = ProfileDescriptor.create()
        .setShortName(SHORT_NAME)
        .setName(FULL_NAME)
        .setDescription(DESCRIPTION)
        .setStartUrl(START_URL)
        .setDisplay(ProfileDescriptor.Display.STANDALONE)
        .setThemeColor(THEME_COLOR)
        .setBackgroundColor(BACKGROUND_COLOR).setBase(BASE_URL)
        .setOrientation(ProfileDescriptor.Orientation.LANDSCAPE)
        .setCategories(CATEGORIES)
        .setId(APP_ID)
        .build();
    // @formatter:on

    assertEquals(SHORT_NAME, descriptor.getShortName());
    assertEquals(FULL_NAME, descriptor.getName());
    assertEquals(DESCRIPTION, descriptor.getDescription());

    assertEquals(START_URL, descriptor.getStartUrl());
    assertEquals(ProfileDescriptor.Display.STANDALONE, descriptor.getDisplay());
    assertEquals(THEME_COLOR, descriptor.getThemeColor());
    assertEquals(BACKGROUND_COLOR, descriptor.getBackgroundColor());
    assertEquals(BASE_URL, descriptor.getBase());
    assertEquals(ProfileDescriptor.Orientation.LANDSCAPE, descriptor.getOrientation());
    assertEquals(CATEGORIES, descriptor.getCategories());
    assertEquals(APP_ID, descriptor.getId());
    assertEquals(0, descriptor.getIcons().size());
    assertEquals(0, descriptor.getScreenshots().size());
  }

  @Test
  void shouldConvertToJson() {
    // @formatter:off
    ProfileDescriptor descriptor = ProfileDescriptor.create()
        .setShortName(SHORT_NAME)
        .setName(FULL_NAME)
        .setDescription(DESCRIPTION)
        .setStartUrl(START_URL)
        .setDisplay(ProfileDescriptor.Display.STANDALONE)
        .setThemeColor(THEME_COLOR)
        .setBackgroundColor(BACKGROUND_COLOR)
        .setBase(BASE_URL)
        .setOrientation(ProfileDescriptor.Orientation.LANDSCAPE)
        .setCategories(CATEGORIES)
        .setId(APP_ID)
        .build();
    // @formatter:on

    JsonObject expectedJson = new JsonObject();
    expectedJson.addProperty("short_name", SHORT_NAME);
    expectedJson.addProperty("name", FULL_NAME);
    expectedJson.addProperty("description", DESCRIPTION);
    expectedJson.addProperty("start_url", START_URL);
    expectedJson.addProperty("display", "standalone");
    expectedJson.addProperty("theme_color", THEME_COLOR);
    expectedJson.addProperty("background_color", BACKGROUND_COLOR);
    expectedJson.addProperty("base", BASE_URL);
    expectedJson.addProperty("orientation", "landscape");
    expectedJson.add("categories",
        JsonParser.parseString("[\"category1\",\"category2\"]").getAsJsonArray());
    expectedJson.addProperty("id", APP_ID);
    expectedJson.add("icons", JsonParser.parseString("[]").getAsJsonArray());
    expectedJson.add("screenshots", JsonParser.parseString("[]").getAsJsonArray());

    JsonObject actualJson =
        JsonParser.parseString(descriptor.toJson().toString()).getAsJsonObject();
    for (String key : expectedJson.keySet()) {
      if (key.equals("base")) {
        continue;
      }

      assertEquals(expectedJson.get(key), actualJson.get(key), "Mismatch for key: " + key
          + " expected: " + expectedJson.get(key) + " actual: " + actualJson.get(key));
    }
  }
}
