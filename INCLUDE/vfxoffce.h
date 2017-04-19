**************************************************************
* Outlook Header File                                OUTLOOK.H
**************************************************************

**************************************
* OlActionCopyLike
* Constant						Value
**************************************
#define olReply					0
#define olReplyAll				1
#define olForward				2
#define olReplyFolder			3
#define olRespond				4

**************************************
* OlActionReplyStyle
* Constant						Value
**************************************
#define olOmitOriginalText		0
#define olEmbedOriginalItem		1
#define olIncludeOriginalText	2
#define olIndentOriginalText	3

**************************************
* OlActionResponseStyle
* Constant						Value
**************************************
#define olOpen					0
#define olSend					1
#define olPrompt				2

**************************************
* OlActionShowOn
* Constant						Value
**************************************
#define olDontShow				0
#define olMenu					1
#define olMenuAndToolbar		2

**************************************
* OlAttachmentType
* Constant						Value
**************************************
#define olByValue				1
#define olByReference			4
#define olEmbeddedItem			5
#define olOLE					6

**************************************
* OlBusyStatus
* Constant						Value
**************************************
#define olFree					0
#define olTentative				1
#define olBusy					2
#define olOutOfOffice			3

**************************************
* OlDaysOfWeek
* Constant						Value
**************************************
#define olSunday				1
#define olMonday				2
#define olTuesday				4
#define olWednesday				8
#define olThursday				16
#define olFriday				32
#define olSaturday				64

**************************************
* OlDefaultFolders
* Constant						Value
**************************************
#define olFolderDeletedItems	3
#define olFolderOutbox			4
#define olFolderSentMail		5
#define olFolderInbox			6
#define olFolderCalendar		9
#define olFolderContacts		10
#define olFolderJournal			11
#define olFolderNotes			12
#define olFolderTasks			13

**************************************
* OlFlagStatus
* Constant						Value
**************************************
#define olNoFlag				0
#define olFlagComplete			1
#define olFlagMarked			2

**************************************
* OlFolderDisplayMode
* Constant						Value
**************************************
#define olFolderDisplayNormal		0
#define olFolderDisplayFolderOnly	1
#define olFolderDisplayNoNavigation	2

**************************************
* OlFormRegistry
* Constant						Value
**************************************
#define olDefaultRegistry		0
#define olPersonalRegistry		2
#define olFolderRegistry		3
#define olOrganizationRegistry	4

**************************************
* OlGender
* Constant						Value
**************************************
#define olUnspecified			0
#define olFemale				1
#define olMale					2

**************************************
* OlImportance
* Constant						Value
**************************************
#define olImportanceLow			0
#define olImportanceNormal		1
#define olImportanceHigh		2

**************************************
* OlInspectorClose
* Constant						Value
**************************************
#define olSave					0
#define olDiscard				1
#define olPromptForSave			2

**************************************
* OlItems
* Constant						Value
**************************************
#define olMailItem				0
#define olAppointmentItem		1
#define olContactItem			2
#define olTaskItem				3
#define olJournalItem			4
#define olNoteItem				5
#define olPostItem				6

**************************************
* OlJournalRecipientType
* Constant						Value
**************************************
#define olAssociatedContact		1

**************************************
* OlMailingAddress
* Constant						Value
**************************************
#define olNone					0
#define olHome					1
#define olBusiness				2
#define olOther					3

**************************************
* OlMailRecipientType
* Constant						Value
**************************************
#define olOriginator			0
#define olTo					1
#define olCC					2
#define olBCC					3

**************************************
* OlMeetingRecipientType
* Constant						Value
**************************************
#define olOrganizer				0
#define olRequired				1
#define olOptional				2
#define olResource				3

**************************************
* OlMeetingResponse
* Constant						Value
**************************************
#define olMeetingTentative		2
#define olMeetingAccepted		3
#define olMeetingDeclined		4

**************************************
* OlMeetingStatus
* Constant						Value
**************************************
#define olNonMeeting			0
#define olMeeting				1
#define olMeetingReceived		3
#define olMeetingCanceled		5

**************************************
* OlNoteColor
* Constant						Value
**************************************
#define olBlue					0
#define olGreen					1
#define olPink					2
#define olYellow				3
#define olWhite					4

**************************************
* OlRecurrenceType
* Constant						Value
**************************************
#define olRecursDaily			0
#define olRecursWeekly			1
#define olRecursMonthly			2
#define olRecursMonthNth		3
#define olRecursYearly			5
#define olRecursYearNth			6

**************************************
* OlRemoteStatus
* Constant						Value
**************************************
#define olRemoteStatusNone		0
#define olUnMarked				1
#define olMarkedForDownload		2
#define olMarkedForCopy			3
#define olMarkedForDelete		4

**************************************
* OlResponseStatus
* Constant						Value
**************************************
#define olResponseNone			0
#define olResponseOrganized		1
#define olResponseTentative		2
#define olResponseAccepted		3
#define olResponseDeclined		4
#define olResponseNotResponded	5

**************************************
* OlSaveAsType
* Constant						Value
**************************************
#define olTXT					0
#define olRTF					1
#define olTemplate				2
#define olMSG					3
#define olDoc					4

**************************************
* OlSensitivity
* Constant						Value
**************************************
#define olNormal				0
#define olPersonal				1
#define olPrivate				2
#define olConfidential			3

**************************************
* OlTaskDelegationState
* Constant						Value
**************************************
#define olTaskNotDelegated			0
#define olTaskDelegationUnknown		1
#define olTaskDelegationAccepted	2
#define olTaskDelegationDeclined	3

**************************************
* OlTaskOwnership
* Constant						Value
**************************************
#define olNewTask				0
#define olDelegatedTask			1
#define olOwnTask				2

**************************************
* OlTaskRecipientType
* Constant						Value
**************************************
#define olUpdate				1
#define olFinalStatus			2

**************************************
* OlTaskResponse
* Constant						Value
**************************************
#define olTaskSimple			0
#define olTaskAssign			1
#define olTaskAccept			2
#define olTaskDecline			3

**************************************
* OlTaskStatus
* Constant						Value
**************************************
#define olTaskNotStarted		0
#define olTaskInProgress		1
#define olTaskComplete			2
#define olTaskWaiting			3
#define olTaskDeferred			4

**************************************
* OlTrackingStatus
* Constant						Value
**************************************
#define olTrackingNone			0
#define olTrackingDelivered		1
#define olTrackingNotDelivered	2
#define olTrackingNotRead		3
#define olTrackingRecallFailure	4
#define olTrackingRecallSuccess	5
#define olTrackingRead			6
#define olTrackingReplied		7

**************************************
* OlUserPropertyType
* Constant						Value
**************************************
#define olText					1
#define olNumber				3
#define olDateTime				5
#define olYesNo					6
#define olDuration				7
#define olKeywords				11
#define olPercent				12
#define olCurrency				14
#define olFormula				18
#define olCombination			19

**************************************
* cBuisenessGraphic
* Constant						Value
**************************************
#define	VtChChartType3dBar			0
#define VtChChartType2dBar			1
#define VtChChartType3dLine			2
#define VtChChartType2dLine			3
#define VtChChartType3dArea			4
#define VtChChartType2dArea			5
#define VtChChartType3dStep			6
#define VtChChartType2dStep			7
#define VtChChartType3dCombination	8
#define VtChChartType2dCombination	9
#define VtChChartType2dPie			14
#define VtChChartType2dXY			16

**************************************
* Other
* Constant						Value
**************************************
#define	wdPaneNone					0
#define	wdNormalView				1
#define	wdOutlineView				2
#define	wdMasterView				5
#define	wdPageView					3
#define	wdSeekCurrentPageHeader		9
#define	wdAlignParagraphCenter		1
#define	wdSeekCurrentPageFooter		10
#define	wdSeekMainDocument			0
