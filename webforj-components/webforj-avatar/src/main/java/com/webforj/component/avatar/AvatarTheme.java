package com.webforj.component.avatar;

import com.google.gson.annotations.SerializedName;
import com.webforj.component.ThemeBase;

/**
 * Supported avatar themes as defined by the Dwc Theme Engine.
 *
 * @author Hyyan Abo Fakher
 * @since 25.11
 */
public enum AvatarTheme implements ThemeBase {
  /**
   * The danger theme as defined by the Dwc Theme Engine.
   **/
  @SerializedName("danger")
  DANGER,
  /**
   * The outlined-danger theme as defined by the Dwc Theme Engine.
   **/
  @SerializedName("outlined-danger")
  OUTLINED_DANGER,
  /**
   * The default theme as defined by the Dwc Theme Engine.
   **/
  @SerializedName("default")
  DEFAULT,
  /**
   * The outlined-default theme as defined by the Dwc Theme Engine.
   **/
  @SerializedName("outlined-default")
  OUTLINED_DEFAULT,
  /**
   * The gray theme as defined by the Dwc Theme Engine.
   **/
  @SerializedName("gray")
  GRAY,
  /**
   * The outlined-gray theme as defined by the Dwc Theme Engine.
   **/
  @SerializedName("outlined-gray")
  OUTLINED_GRAY,
  /**
   * The info theme as defined by the Dwc Theme Engine.
   **/
  @SerializedName("info")
  INFO,
  /**
   * The outlined-info theme as defined by the Dwc Theme Engine.
   **/
  @SerializedName("outlined-info")
  OUTLINED_INFO,
  /**
   * The primary theme as defined by the Dwc Theme Engine.
   **/
  @SerializedName("primary")
  PRIMARY,
  /**
   * The outlined-primary theme as defined by the Dwc Theme Engine.
   **/
  @SerializedName("outlined-primary")
  OUTLINED_PRIMARY,
  /**
   * The success theme as defined by the Dwc Theme Engine.
   **/
  @SerializedName("success")
  SUCCESS,
  /**
   * The outlined-success theme as defined by the Dwc Theme Engine.
   **/
  @SerializedName("outlined-success")
  OUTLINED_SUCCESS,
  /**
   * The warning theme as defined by the Dwc Theme Engine.
   **/
  @SerializedName("warning")
  WARNING,
  /**
   * The outlined-warning theme as defined by the Dwc Theme Engine.
   **/
  @SerializedName("outlined-warning")
  OUTLINED_WARNING;
}
