INTRODUCTION
================================================================================
Use this Python class to create a monthly calendar. 
It's easy to use and highly customizable, supports multiple languages,
and you can choose whether weeks start with Saturday, Sunday,
Monday, or any other day. Of course you can create as many calendars 
as you like.

In 1582, Gregory XIII made a calendar reformation 
(advanced leap year rules + 10 days have been dropped in October)
and created the Gregorian calendar, which is in use since then. 
Therefore HTML-Calendar applies the Gregorian
calendar for years > 1582, and the old Julian calendar for years <= 1582.

LICENSE
================================================================================
This script is freeware for non-commercial use. If you like it, 
please feel free to make a donation!
However, if you intend to use the script in a commercial project, please donate at least EUR 4.
You can make a donation on my website: http://www.gerd-tentler.de/tools/pycalendar/.

USAGE
================================================================================
Import the calendar module into your Python script and create a new instance of the MonthlyCalendar class:

  import calendar
  myCal = calendar.MonthlyCalendar()              # view current month

If you don't want to view the current month, you can set year and month like this:

  myCal = calendar.MonthlyCalendar(2004, 12)      # view December 2004
  myCal = calendar.MonthlyCalendar(2004)          # view January 2004
  myCal = calendar.MonthlyCalendar(month = 12)    # view December of current year

You can also set year and month by using the according properties:

  myCal = calendar.MonthlyCalendar()
  myCal.year = 2004
  myCal.month = 12

NOTE: I'm using my own functions to calculate the dates. Thus the calendar range is from year 1 to year 3999 and
not affected by any Python or operating system restrictions. Also note that this script applies both, the Gregorian
and the Julian calendar, so it can be used to view historically correct dates.

Adapt the configuration to your needs:

  myCal.tFontFace = "Comic Sans MS"               # change font face for title (month)
  myCal.hFontSize = 3                             # change font size for heading (weekdays)
  myCal.dFontColor = "#808080"                    # change font color for days
  myCal.offset = 2                                # start week with Monday
  myCal.link = "path/to/my/page.py"               # set page to link to when day is clicked
  ...

Use the function viewEvent() to add events to the calendar. It takes the following arguments:

  viewEvent(start day, end day, color, title, [link])

Example:

  # view seminar "How to use HTML-Calendar" from 6th to 8th with color #E0E0FF
  myCal.viewEvent(6, 8, "#E0E0FF", "Seminar &quot;How to use HTML-Calendar&quot;")

  # view trip to Hawaii from 15th to 19th with color #D0FFD0 and link
  myCal.viewEvent(15, 19, "#D0FFD0", "Trip to Hawaii!", "/trips/hawaii/index.py")

Finally create the calendar:

  print myCal.create()

Source code + examples available at http://www.gerd-tentler.de/tools/pycalendar/.
================================================================================
