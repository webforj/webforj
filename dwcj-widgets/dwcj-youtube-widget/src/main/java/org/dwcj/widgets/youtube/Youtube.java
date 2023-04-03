package org.dwcj.widgets.youtube;

import java.util.HashMap;
import java.util.Map;

import org.dwcj.annotation.InlineStyleSheet;
import org.dwcj.component.HasAttribute;
import org.dwcj.component.HasClassName;
import org.dwcj.component.HasStyle;
import org.dwcj.component.webcomponent.PropertyDescriptor;
import org.dwcj.component.webcomponent.WebComponent;
import org.dwcj.component.webcomponent.annotations.HtmlViewAttribute;
import org.dwcj.component.webcomponent.annotations.NodeAttribute;
import org.dwcj.component.webcomponent.annotations.NodeName;
import org.dwcj.component.window.AbstractWindow;

/**
 * The Youtube component is a component that allows you to embed a youtube
 * video in your application.
 * 
 * For instance, to embed a youtube video in your application you can do the
 * following:
 * 
 * <pre>
 * {@code
 * Youtube youtube = new Youtube();
 * youtube.setVideoId("youtube-video-id");
 * youtube.setAutoPlay(true);
 * youtube.setLoop(true);
 * youtube.setControls(false);
 * youtube.setMute(true);
 * }
 * </pre>
 * 
 * @author Hyyan Abo Fakher
 */
@HtmlViewAttribute(name = "dwcj-youtube-container")
@NodeName("iframe")
@NodeAttribute(name = "dwcj-youtube")
@NodeAttribute(name = "frameborder", value = "0")
@NodeAttribute(name = "allow", value = "accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share")
@NodeAttribute(name = "allowfullscreen", value = "true")
@InlineStyleSheet(id = "dwcj-youtube-widget", once = true, value = /* css */ """
    [dwcj-youtube-container] {
      width: 100%;
      /* Keep it the right aspect-ratio */
      aspect-ratio: 16/9;
      overflow: hidden !important;
    }

    [dwcj-youtube-container] iframe {
      /* Extend it beyond the viewport... */
      width: 300%;
      height: 100%;
      /* ...and bring it back again */
      margin-left: -100%;
    }
    """)
public class Youtube extends WebComponent implements HasClassName, HasStyle, HasAttribute {

  /** The type of the content that will load in the player. */
  public enum ListType {
    /**
     * The list parameter value identifies the YouTube channel whose uploaded videos
     * will be loaded.
     */
    USER_UPLOADS("user_uploads"),
    /**
     * Specifies a YouTube playlist ID. In the parameter value, you need to prepend
     * the playlist ID with the letters PL as shown in the example below.
     * 
     * <pre>
     * {@code
     * player.setListType(ListType.PLAYLIST);
     * player.setList("PLC77007E23FF423C6");
     * }
     * </pre>
     */
    PLAYLIST("playlist");

    private String value;

    private ListType(String value) {
      this.value = value;
    }

    /**
     * Get the value of the enum
     * 
     * @return the value of the enum
     */
    public String getValue() {
      return this.value;
    }

    /**
     * Get the enum from the value
     * 
     * @param value the value of the enum
     * @return the enum
     */
    public static ListType fromValue(String value) {
      for (ListType list : ListType.values()) {
        if (list.getValue().equals(value)) {
          return list;
        }
      }

      return null;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String toString() {
      return this.value;
    }
  }

  // Properties
  // @see https://developers.google.com/youtube/player_parameters#Parameters
  private final PropertyDescriptor<String> SRC = PropertyDescriptor.property("src", "");
  private String videoId = "";
  private boolean autoplay = false;
  private String captionLanguage = "en";
  private boolean showCaption = false;
  private boolean controls = true;
  private boolean disableKeyboard = false;
  private int end = 0;
  private boolean fullScreenButton = true;
  private String language = "en";
  private int showAnnotations = 1;
  private String list = "";
  private ListType listType = null;
  private boolean loop = false;
  private boolean modestBranding = false;
  private String playlist = "";
  private boolean playsInline = false;
  private int start = 0;
  private boolean mute = false;

  /**
   * Create a new youtube component
   */
  public Youtube() {
    super();
  }

  /**
   * Create a new youtube component with the given id
   * 
   * @param id the id of the youtube video
   */
  public Youtube(String id) {
    this();
    this.setVideoId(id);
  }

  /**
   * Specifies the ID of the YouTube video to be played.
   * 
   * @param id the id of the youtube video
   * @return the youtube component
   */
  public Youtube setVideoId(String id) {
    this.videoId = id;
    this.updateSrc();
    return this;
  }

  /**
   * Get the id of the youtube video
   * 
   * @return the id of the youtube video
   */
  public String getVideoId() {
    return this.videoId;
  }

  /**
   * Enable or disable the autoplay.
   * 
   * Specifies whether the initial video will automatically start to play when the
   * player loads.
   * 
   * @param autoplay the autoplay property
   * @return the youtube component
   */
  public Youtube setAutoPlay(Boolean autoplay) {
    this.autoplay = autoplay;
    this.updateSrc();
    return this;
  }

  /**
   * Check if the autoplay is enabled
   * 
   * @return true if the autoplay is enabled, false otherwise
   */
  public Boolean isAutoPlay() {
    return this.autoplay;
  }

  /**
   * Specifies the default language that the player will use to display captions.
   * 
   * Set the option's value to an
   * <a href="http://www.loc.gov/standards/iso639-2/php/code_list.php"> ISO 639-1
   * two-letter language code.</a>
   * 
   * If you use this option and also set the {@link #setCaption(Boolean)
   * showCaption } then the player will show captions in the specified language
   * when the player loads.
   * If you do not also set the {@link #setCaption(Boolean) showCaption }
   * option, then captions will not
   * display by default, but will display in the specified language if the user
   * opts to turn captions on.
   * 
   * @param language the default language
   * @return the youtube component
   */
  public Youtube setCaptionLanguage(String language) {
    this.captionLanguage = language;
    this.updateSrc();
    return this;
  }

  /**
   * Get the default language
   * 
   * @return the default language
   */
  public String getCaptionLanguage() {
    return this.captionLanguage;
  }

  /**
   * When enabled causes closed captions to be shown by default, even if the user
   * has turned captions off. The default behavior is based on user preference.
   * 
   * @param showCaption the load caption property
   * @return the youtube component
   */
  public Youtube setCaption(Boolean showCaption) {
    this.showCaption = showCaption;
    this.updateSrc();
    return this;
  }

  /**
   * Check if the load caption is enabled
   * 
   * @return true if the load caption is enabled, false otherwise
   */
  public Boolean isCaption() {
    return this.showCaption;
  }

  /**
   * Show or hide the player controls.
   * 
   * @param controls when true, the player controls are shown, hidden otherwise.
   * @return the youtube component
   */
  public Youtube setControls(Boolean controls) {
    this.controls = controls;
    this.updateSrc();
    return this;
  }

  /**
   * Check if the controls are shown
   * 
   * @return true if the controls are shown, false otherwise
   */
  public Boolean isControls() {
    return this.controls;
  }

  /**
   * Enable or disable the keyboard controls.
   * 
   * @param disableKeyboard when true causes the player to not respond to keyboard
   *                        controls.
   * @return the youtube component
   */
  public Youtube setDisableKeyboard(Boolean disableKeyboard) {
    this.disableKeyboard = disableKeyboard;
    this.updateSrc();
    return this;
  }

  /**
   * Check if keyboard interactions are disabled
   * 
   * @return the disableKeyboard property
   */
  public Boolean isDisableKeyboard() {
    return this.disableKeyboard;
  }

  /**
   * Specifies the time, measured in seconds from the start of the video, when the
   * player should stop playing the video
   * 
   * @param end the end time
   * @return the youtube component
   */
  public Youtube setEnd(int end) {
    this.end = end;
    this.updateSrc();
    return this;
  }

  /**
   * Get the end time
   * 
   * @return the end time
   */
  public int getEnd() {
    return this.end;
  }

  /**
   * Enable or disable the fullscreen button.
   * 
   * @param fullScreen when false, prevents the fullscreen button from displaying
   *                   in the player. by default the button will display.
   * @return the youtube component
   */
  public Youtube setFullScreenButton(Boolean fullScreen) {
    this.fullScreenButton = fullScreen;
    this.updateSrc();
    return this;
  }

  /**
   * Check if the fullscreen button is enabled
   * 
   * @return true if the fullscreen button is enabled, false otherwise
   */
  public Boolean isFullScreenButton() {
    return this.fullScreenButton;
  }

  /**
   * Sets the player's interface language. The option value is an
   * <a href="http://www.loc.gov/standards/iso639-2/php/code_list.php">ISO 639-1
   * two-letter language code</a> or a fully specified locale. For example,
   * <code>fr</code> and
   * </code>fr-ca</code> are both valid values. Other language input codes, such
   * as <code>IETF</code>
   * language tags (BCP 47) might also be handled properly.
   * 
   * The interface language is used for tooltips in the player and also affects
   * the default caption track. Note that YouTube might select a different caption
   * track language for a particular user based on the user's individual language
   * preferences and the availability of caption tracks.
   * 
   * @param language the language
   * @return the youtube component
   */
  public Youtube setLanguage(String language) {
    this.language = language;
    this.updateSrc();
    return this;
  }

  /**
   * Get the language
   * 
   * @return the language
   * @see #setLanguage(String)
   */
  public String getLanguage() {
    return this.language;
  }

  /**
   * Enable or disable the video annotations.
   * 
   * @param showAnnotations true to show the annotations, false otherwise to hide.
   * @return the youtube component
   */
  public Youtube setShowAnnotations(Boolean showAnnotations) {
    this.showAnnotations = Boolean.TRUE.equals(showAnnotations) ? 1 : 3;
    this.updateSrc();
    return this;
  }

  /**
   * Check if the annotations are enabled
   * 
   * @return true if the annotations are enabled, false otherwise
   */
  public Boolean isShowAnnotations() {
    return this.showAnnotations == 1;
  }

  /**
   * The list option, in conjunction with the {@link #setListType(ListType)}
   * option, identifies
   * the content that will load in the player.
   * 
   * @param list the list
   * @return the youtube component
   */
  public Youtube setList(String list) {
    this.list = list;
    this.updateSrc();
    return this;
  }

  /**
   * Get the list
   * 
   * @return the list
   */
  public String getList() {
    return this.list;
  }

  /**
   * The listType option, in conjunction with the {@link #setList(String) list}
   * option, identifies the content that will load in the player.
   * 
   * @param listType the list type
   * @return the youtube component
   */
  public Youtube setListType(ListType listType) {
    this.listType = listType;
    this.updateSrc();
    return this;
  }

  /**
   * Get the list type
   * 
   * @return the list type
   */
  public ListType getListType() {
    return this.listType;
  }

  /**
   * Automatically play the video again and again.
   * 
   * <p>
   * In the case of a single video player, enabling the option causes the player
   * to play
   * the initial video again and again. In the case of a playlist player the
   * player plays the entire playlist and then starts again at
   * the first video.
   * </p>
   * 
   * @param loop the loop property
   * @return the youtube component
   */
  public Youtube setLoop(Boolean loop) {
    this.loop = loop;
    this.updateSrc();
    return this;
  }

  /**
   * Check if the loop is enabled
   * 
   * @return true if the loop is enabled, false otherwise
   */
  public Boolean isLoop() {
    return this.loop;
  }

  /**
   * A Boolean value that indicates whether the YouTube player does not show a
   * YouTube logo.
   * 
   * When true then prevents the YouTube logo from displaying in the control bar.
   * Note that a small YouTube text label will still display in the upper-right
   * corner of a paused video when the user's mouse pointer hovers over the
   * player.
   * 
   * @param modestBranding when true, the YouTube logo is not shown, shown
   *                       otherwise.
   * @return the youtube component
   */
  public Youtube setModestBranding(Boolean modestBranding) {
    this.modestBranding = modestBranding;
    this.updateSrc();
    return this;
  }

  /**
   * Get the modestBranding property
   * 
   * @return the modestBranding property
   */
  public Boolean isModestBranding() {
    return this.modestBranding;
  }

  /**
   * Specifies a comma-separated list of video IDs to play.
   * 
   * If you specify a value, the first video that plays will be the video id
   * specified, and the videos specified in the playlist parameter will play
   * thereafter.
   * 
   * @param playlist the playlist property
   * @return the youtube component
   */
  public Youtube setPlaylist(String playlist) {
    this.playlist = playlist;
    this.updateSrc();
    return this;
  }

  /**
   * Get the list of videos to play
   * 
   * @return the playlist property
   */
  public String getPlaylist() {
    return this.playlist;
  }

  /**
   * Controls whether videos play inline or fullscreen on iOS.
   * 
   * when false, then results in fullscreen playback. This is currently the
   * default value.
   * when true, then results in inline playback for mobile browsers and for
   * WebViews created with the <code>allowsInlineMediaPlayback</code> property set
   * to YES.
   * 
   * @param playsinline true to play inline, false otherwise
   * @return the youtube component
   */
  public Youtube setPlaysInline(Boolean playsinline) {
    this.playsInline = playsinline;
    this.updateSrc();
    return this;
  }

  /**
   * Get the playsinline property
   * 
   * @return the playsinline property
   */
  public Boolean isPlaysInline() {
    return this.playsInline;
  }

  /**
   * Set the start time
   * 
   * Causes the player to begin playing the video at the given number of seconds
   * from the start of the video.
   * 
   * @param start the start time
   * @return the youtube component
   */
  public Youtube setStart(int start) {
    this.start = start;
    this.updateSrc();
    return this;
  }

  /**
   * Get the start time
   * 
   * @return the start time
   */
  public int getStart() {
    return this.start;
  }

  /**
   * Set the mute property
   * 
   * @param mute the mute property
   * @return the youtube component
   */
  public Youtube setMute(Boolean mute) {
    this.mute = mute;
    this.updateSrc();
    return this;
  }

  /**
   * Get the mute property
   * 
   * @return the mute property
   */
  public Boolean getMute() {
    return this.mute;
  }

  /**
   * Get the URL of the video
   * 
   * @return the URL of the video
   */
  public String getURL() {
    String url = "https://www.youtube.com/embed/" + this.videoId;
    HashMap<String, String> params = new HashMap<String, String>();

    params.put("autoplay", this.autoplay ? "1" : "0");
    params.put("cc_load_policy", this.showCaption ? "1" : "0");
    params.put("controls", this.controls ? "1" : "0");
    params.put("disablekb", this.disableKeyboard ? "1" : "0");
    params.put("fs", this.fullScreenButton ? "1" : "0");
    params.put("iv_load_policy", String.valueOf(this.showAnnotations));
    params.put("loop", this.loop ? "1" : "0");
    params.put("modestbranding", this.modestBranding ? "1" : "0");
    params.put("playsinline", this.playsInline ? "1" : "0");
    params.put("mute", this.mute ? "1" : "0");

    if (this.captionLanguage != null && !this.captionLanguage.isEmpty()) {
      params.put("cc_lang_pref", this.captionLanguage);
    }

    if (this.end > 0) {
      params.put("end", String.valueOf(this.end));
    }

    if (this.start > 0) {
      params.put("start", String.valueOf(this.start));
    }

    if (this.language != null && !this.language.isEmpty()) {
      params.put("hl", this.language);
    }

    if (this.list != null && !this.list.isEmpty()) {
      params.put("list", this.list);
    }

    if (this.listType != null) {
      params.put("listType", this.listType.getValue());
    }

    if (this.playlist != null && !this.playlist.isEmpty()) {
      params.put("playlist", this.playlist);
    }

    if (!params.isEmpty()) {
      StringBuilder builder = new StringBuilder(url + "?");
      for (Map.Entry<String, String> entry : params.entrySet()) {
        builder.append(entry.getKey() + "=" + entry.getValue() + "&");
      }

      url = builder.toString();
    }

    return url;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Youtube addClassName(String className) {
    addComponentClassName(className);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Youtube removeClassName(String className) {
    removeComponentClassName(className);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Youtube setStyle(String property, String value) {
    setComponentStyle(property, value);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getStyle(String property) {
    return getComponentStyle(property);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Youtube removeStyle(String property) {
    removeComponentStyle(property);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getComputedStyle(String property) {
    return getComponentComputedStyle(property);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getAttribute(String attribute) {
    return getComponentAttribute(attribute);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Youtube setAttribute(String attribute, String value) {
    setComponentAttribute(attribute, value);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Youtube removeAttribute(String attribute) {
    removeComponentAttribute(attribute);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void onAttach(AbstractWindow panel) {
    super.onAttach(panel);
    updateSrc();
  }

  /**
   * Build and set the src attribute
   */
  private void updateSrc() {
    if (!isAttached())
      return;

    if (Boolean.TRUE.equals(isControls())) {
      setComponentStyle("pointer-events", "auto");
    } else {
      setComponentStyle("pointer-events", "none");
    }

    set(SRC, getURL());
  }
}
