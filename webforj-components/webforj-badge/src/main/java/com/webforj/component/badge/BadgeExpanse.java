package com.webforj.component.badge;

import com.google.gson.annotations.SerializedName;
import com.webforj.component.ExpanseBase;

/**
 * Supported badge expanses as defined by the Dwc Theme Engine.
 *
 * @author Hyyan Abo Fakher
 * @since 25.12
 */
public enum BadgeExpanse implements ExpanseBase {

  @SerializedName("3xl")
  XXXLARGE,

  @SerializedName("2xl")
  XXLARGE,

  @SerializedName("xl")
  XLARGE,

  @SerializedName("l")
  LARGE,

  @SerializedName("m")
  MEDIUM,

  @SerializedName("s")
  SMALL,

  @SerializedName("xs")
  XSMALL,

  @SerializedName("2xs")
  XXSMALL,

  @SerializedName("3xs")
  XXXSMALL;
}
