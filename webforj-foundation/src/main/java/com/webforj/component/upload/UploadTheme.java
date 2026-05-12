package com.webforj.component.upload;

import com.google.gson.annotations.SerializedName;
import com.webforj.component.ThemeBase;

/**
 * Supported {@link Upload} themes as defined by the Dwc Theme Engine.
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public enum UploadTheme implements ThemeBase {
  /**
   * The default theme as defined by the Dwc Theme Engine.
   */
  @SerializedName("default")
  DEFAULT,
  /**
   * The primary theme as defined by the Dwc Theme Engine.
   */
  @SerializedName("primary")
  PRIMARY,
  /**
   * The success theme as defined by the Dwc Theme Engine.
   */
  @SerializedName("success")
  SUCCESS,
  /**
   * The warning theme as defined by the Dwc Theme Engine.
   */
  @SerializedName("warning")
  WARNING,
  /**
   * The danger theme as defined by the Dwc Theme Engine.
   */
  @SerializedName("danger")
  DANGER,
  /**
   * The info theme as defined by the Dwc Theme Engine.
   */
  @SerializedName("info")
  INFO,
  /**
   * The gray theme as defined by the Dwc Theme Engine.
   */
  @SerializedName("gray")
  GRAY,
  /**
   * The {@code outlined-default} theme as defined by the Dwc Theme Engine.
   */
  @SerializedName("outlined-default")
  OUTLINED_DEFAULT,
  /**
   * The {@code outlined-primary} theme as defined by the Dwc Theme Engine.
   */
  @SerializedName("outlined-primary")
  OUTLINED_PRIMARY,
  /**
   * The {@code outlined-success} theme as defined by the Dwc Theme Engine.
   */
  @SerializedName("outlined-success")
  OUTLINED_SUCCESS,
  /**
   * The {@code outlined-warning} theme as defined by the Dwc Theme Engine.
   */
  @SerializedName("outlined-warning")
  OUTLINED_WARNING,
  /**
   * The {@code outlined-danger} theme as defined by the Dwc Theme Engine.
   */
  @SerializedName("outlined-danger")
  OUTLINED_DANGER,
  /**
   * The {@code outlined-info} theme as defined by the Dwc Theme Engine.
   */
  @SerializedName("outlined-info")
  OUTLINED_INFO,
  /**
   * The {@code outlined-gray} theme as defined by the Dwc Theme Engine.
   */
  @SerializedName("outlined-gray")
  OUTLINED_GRAY;
}
