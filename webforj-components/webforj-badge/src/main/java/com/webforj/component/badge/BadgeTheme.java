package com.webforj.component.badge;

import com.google.gson.annotations.SerializedName;
import com.webforj.component.ThemeBase;

/**
 * Supported badge themes as defined by the Dwc Theme Engine.
 *
 * @author Hyyan Abo Fakher
 * @since 25.12
 */
public enum BadgeTheme implements ThemeBase {

  @SerializedName("default")
  DEFAULT,

  @SerializedName("primary")
  PRIMARY,

  @SerializedName("success")
  SUCCESS,

  @SerializedName("warning")
  WARNING,

  @SerializedName("danger")
  DANGER,

  @SerializedName("info")
  INFO,

  @SerializedName("gray")
  GRAY,

  @SerializedName("outlined-default")
  OUTLINED_DEFAULT,

  @SerializedName("outlined-primary")
  OUTLINED_PRIMARY,

  @SerializedName("outlined-success")
  OUTLINED_SUCCESS,

  @SerializedName("outlined-warning")
  OUTLINED_WARNING,

  @SerializedName("outlined-danger")
  OUTLINED_DANGER,

  @SerializedName("outlined-info")
  OUTLINED_INFO,

  @SerializedName("outlined-gray")
  OUTLINED_GRAY;
}
