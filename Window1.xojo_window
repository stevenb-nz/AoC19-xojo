#tag Window
Begin Window Window1
   BackColor       =   &cFFFFFF00
   Backdrop        =   0
   CloseButton     =   True
   Compatibility   =   ""
   Composite       =   False
   Frame           =   0
   FullScreen      =   False
   FullScreenButton=   False
   HasBackColor    =   False
   Height          =   800
   ImplicitInstance=   True
   LiveResize      =   True
   MacProcID       =   0
   MaxHeight       =   32000
   MaximizeButton  =   True
   MaxWidth        =   32000
   MenuBar         =   1354051583
   MenuBarVisible  =   True
   MinHeight       =   64
   MinimizeButton  =   True
   MinWidth        =   64
   Placement       =   0
   Resizeable      =   True
   Title           =   "Untitled"
   Visible         =   True
   Width           =   1200
   Begin TextArea inputArea
      AcceptTabs      =   False
      Alignment       =   0
      AutoDeactivate  =   True
      AutomaticallyCheckSpelling=   True
      BackColor       =   &cFFFFFF00
      Bold            =   False
      Border          =   True
      DataField       =   ""
      DataSource      =   ""
      Enabled         =   True
      Format          =   ""
      Height          =   760
      HelpTag         =   ""
      HideSelection   =   True
      Index           =   -2147483648
      Italic          =   False
      Left            =   20
      LimitText       =   0
      LineHeight      =   0.0
      LineSpacing     =   1.0
      LockBottom      =   False
      LockedInPosition=   False
      LockLeft        =   True
      LockRight       =   False
      LockTop         =   True
      Mask            =   ""
      Multiline       =   True
      ReadOnly        =   False
      Scope           =   0
      ScrollbarHorizontal=   False
      ScrollbarVertical=   True
      Styled          =   True
      TabIndex        =   0
      TabPanelIndex   =   0
      TabStop         =   True
      Text            =   "193651-649729\n"
      TextColor       =   &c00000000
      TextFont        =   "System"
      TextSize        =   0.0
      TextUnit        =   0
      Top             =   20
      Transparent     =   False
      Underline       =   False
      UseFocusRing    =   True
      Visible         =   True
      Width           =   528
   End
   Begin TextArea outputArea
      AcceptTabs      =   False
      Alignment       =   0
      AutoDeactivate  =   True
      AutomaticallyCheckSpelling=   True
      BackColor       =   &cFFFFFF00
      Bold            =   False
      Border          =   True
      DataField       =   ""
      DataSource      =   ""
      Enabled         =   True
      Format          =   ""
      Height          =   760
      HelpTag         =   ""
      HideSelection   =   True
      Index           =   -2147483648
      Italic          =   False
      Left            =   652
      LimitText       =   0
      LineHeight      =   0.0
      LineSpacing     =   1.0
      LockBottom      =   False
      LockedInPosition=   False
      LockLeft        =   True
      LockRight       =   False
      LockTop         =   True
      Mask            =   ""
      Multiline       =   True
      ReadOnly        =   False
      Scope           =   0
      ScrollbarHorizontal=   False
      ScrollbarVertical=   True
      Styled          =   True
      TabIndex        =   1
      TabPanelIndex   =   0
      TabStop         =   True
      Text            =   ""
      TextColor       =   &c00000000
      TextFont        =   "System"
      TextSize        =   0.0
      TextUnit        =   0
      Top             =   20
      Transparent     =   False
      Underline       =   False
      UseFocusRing    =   True
      Visible         =   True
      Width           =   528
   End
   Begin PopupMenu dayPopupMenu
      AutoDeactivate  =   True
      Bold            =   False
      DataField       =   ""
      DataSource      =   ""
      Enabled         =   True
      Height          =   20
      HelpTag         =   ""
      Index           =   -2147483648
      InitialParent   =   ""
      InitialValue    =   "01a\n01b\n02a\n02b\n03a\n03b\n04a\n04b\n05a\n05b\n06a\n06b\n07a\n07b\n08a\n08b\n09a\n09b\n10a\n10b\n11a\n11b\n12a\n12b\n13a\n13b\n14a\n14b\n15a\n15b\n"
      Italic          =   False
      Left            =   560
      ListIndex       =   0
      LockBottom      =   False
      LockedInPosition=   False
      LockLeft        =   True
      LockRight       =   False
      LockTop         =   True
      Scope           =   0
      TabIndex        =   2
      TabPanelIndex   =   0
      TabStop         =   True
      TextFont        =   "System"
      TextSize        =   0.0
      TextUnit        =   0
      Top             =   20
      Transparent     =   False
      Underline       =   False
      Visible         =   True
      Width           =   80
   End
   Begin PushButton runButton
      AutoDeactivate  =   True
      Bold            =   False
      ButtonStyle     =   "0"
      Cancel          =   False
      Caption         =   "Run"
      Default         =   False
      Enabled         =   True
      Height          =   20
      HelpTag         =   ""
      Index           =   -2147483648
      InitialParent   =   ""
      Italic          =   False
      Left            =   560
      LockBottom      =   False
      LockedInPosition=   False
      LockLeft        =   True
      LockRight       =   False
      LockTop         =   True
      Scope           =   0
      TabIndex        =   3
      TabPanelIndex   =   0
      TabStop         =   True
      TextFont        =   "System"
      TextSize        =   0.0
      TextUnit        =   0
      Top             =   52
      Transparent     =   False
      Underline       =   False
      Visible         =   True
      Width           =   80
   End
End
#tag EndWindow

#tag WindowCode
	#tag Method, Flags = &h0
		Function day02a(input As String) As String
		  dim temp(-1) as string
		  dim prog(-1) as Integer
		  dim i,p as integer
		  
		  temp = input.Split(",")
		  for i = 0 to UBound(temp)
		    prog.Append val(temp(i))
		  next
		  p = 0
		  while prog(p) <> 99
		    select case prog(p)
		    case 1
		      prog(prog(p+3)) = prog(prog(p+1)) + prog(prog(p+2))
		    case 2
		      prog(prog(p+3)) = prog(prog(p+1)) * prog(prog(p+2))
		    end select
		    p = p + 4
		  wend
		  
		  return str(prog(0))
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function day02b(input As String) As String
		  dim temp(-1) as string
		  dim prog(-1) as Integer
		  dim i,j,k,p as integer
		  
		  temp = input.Split(",")
		  for k = 0 to UBound(temp)
		    prog.Append val(temp(k))
		  next
		  
		  for i = 0 to 99
		    for j = 0 to 99
		      redim prog(-1)
		      for k = 0 to UBound(temp)
		        prog.Append val(temp(k))
		      next
		      prog(1) = i
		      prog(2) = j
		      p = 0
		      while prog(p) <> 99
		        select case prog(p)
		        case 1
		          prog(prog(p+3)) = prog(prog(p+1)) + prog(prog(p+2))
		        case 2
		          prog(prog(p+3)) = prog(prog(p+1)) * prog(prog(p+2))
		        end select
		        p = p + 4
		      wend
		      if prog(0) = 19690720 then
		        return str(prog(1)*100+prog(2))
		      end
		    next
		  next
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function day03a(input As String) As String
		  dim temp(-1) as string
		  dim wire1(-1) as string
		  dim i,j,p,x,y,minmd as integer
		  dim d As string
		  dim points as new Dictionary
		  
		  temp = input.Split(chr(13))
		  wire1 = temp(0).Split(",")
		  x=0
		  y=0
		  for i = 0 to UBound(wire1)
		    d = left(wire1(i),1)
		    p = val(right(wire1(i),len(wire1(i))-1))
		    for j = 1 to p
		      select case d
		      case "D"
		        y = y -1
		      case "L"
		        x = x - 1
		      case "R"
		        x = x + 1
		      case "U"
		        y = y + 1
		      else
		        MsgBox "error"
		      end select
		      points.Value(str(x)+","+str(y)) = "w1"
		    next
		  next
		  wire1 = temp(1).Split(",")
		  x=0
		  y=0
		  for i = 0 to UBound(wire1)
		    d = left(wire1(i),1)
		    p = val(right(wire1(i),len(wire1(i))-1))
		    for j = 1 to p
		      select case d
		      case "D"
		        y = y -1
		      case "L"
		        x = x - 1
		      case "R"
		        x = x + 1
		      case "U"
		        y = y + 1
		      else
		        MsgBox "error"
		      end select
		      if points.HasKey(str(x)+","+str(y)) then
		        if minmd = 0 then
		          minmd = abs(0-x)+abs(0-y)
		        end
		        if minmd > abs(0-x)+abs(0-y) then
		          minmd = abs(0-x)+abs(0-y)
		        end
		      end
		    next
		  next
		  
		  return str(minmd)
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function day03b(input As String) As String
		  dim temp(-1) as string
		  dim wire1(-1) as string
		  dim i,j,p,x,y,minmd,w1,w2 as integer
		  dim d As string
		  dim points as new Dictionary
		  
		  temp = input.Split(chr(13))
		  wire1 = temp(0).Split(",")
		  x=0
		  y=0
		  for i = 0 to UBound(wire1)
		    d = left(wire1(i),1)
		    p = val(right(wire1(i),len(wire1(i))-1))
		    for j = 1 to p
		      select case d
		      case "D"
		        y = y -1
		      case "L"
		        x = x - 1
		      case "R"
		        x = x + 1
		      case "U"
		        y = y + 1
		      else
		        MsgBox "error"
		      end select
		      w1 = w1 + 1
		      if not points.haskey(str(x)+","+str(y)) then
		        points.value(str(x)+","+str(y)) = w1
		      end
		    next
		  next
		  wire1 = temp(1).Split(",")
		  x=0
		  y=0
		  for i = 0 to UBound(wire1)
		    d = left(wire1(i),1)
		    p = val(right(wire1(i),len(wire1(i))-1))
		    for j = 1 to p
		      select case d
		      case "D"
		        y = y -1
		      case "L"
		        x = x - 1
		      case "R"
		        x = x + 1
		      case "U"
		        y = y + 1
		      else
		        MsgBox "error"
		      end select
		      w2 = w2 + 1
		      if points.HasKey(str(x)+","+str(y)) then
		        if minmd = 0 then
		          minmd = points.value(str(x)+","+str(y))+w2
		        end
		        if minmd > points.value(str(x)+","+str(y))+w2 then
		          minmd = points.value(str(x)+","+str(y))+w2
		        end
		      end
		    next
		  next
		  
		  return str(minmd)
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function day04a(input As String) As String
		  dim temp() as string
		  dim lower, upper as integer
		  dim a,b,c,d,e,f, i as integer
		  dim check as Boolean
		  dim canda,candb,candc,candd,cande,candf as string
		  dim candv,total as integer
		  
		  temp = split(input,"-")
		  lower = val(temp(0))
		  upper = val(temp(1))
		  
		  for a = val(left(str(lower),1)) to val(left(str(upper),1))
		    canda = str(a)
		    for b = a to 9
		      candb = canda + str(b)
		      for c = b to 9
		        candc = candb + str(c)
		        for d = c to 9
		          candd = candc + str(d)
		          for e = d to 9
		            cande = candd + str(e)
		            for f = e to 9
		              candf = cande + str(f)
		              candv = val(candf)
		              if candv >= lower and candv <= upper then
		                check = false
		                for i = 1 to 5
		                  if mid(candf,i,1) = mid(candf,i+1,1) then
		                    check = true
		                  end
		                next
		                if check then
		                  total = total + 1
		                end
		              end
		            next
		          next
		        next
		      Next
		    next
		  next
		  
		  return str(total)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function day04b(input As String) As String
		  dim temp() as string
		  dim digits(9) as integer
		  dim lower, upper as integer
		  dim a,b,c,d,e,f, i as integer
		  dim check as Boolean
		  dim canda,candb,candc,candd,cande,candf as string
		  dim candv,total as integer
		  
		  temp = split(input,"-")
		  lower = val(temp(0))
		  upper = val(temp(1))
		  
		  for a = val(left(str(lower),1)) to val(left(str(upper),1))
		    canda = str(a)
		    for b = a to 9
		      candb = canda + str(b)
		      for c = b to 9
		        candc = candb + str(c)
		        for d = c to 9
		          candd = candc + str(d)
		          for e = d to 9
		            cande = candd + str(e)
		            for f = e to 9
		              candf = cande + str(f)
		              candv = val(candf)
		              if candv >= lower and candv <= upper then
		                check = false
		                for i = 1 to 5
		                  if mid(candf,i,1) = mid(candf,i+1,1) then
		                    check = true
		                  end
		                next
		                if check then
		                  check = false
		                  for i = 0 to 9
		                    digits(i) = 0
		                  next
		                  for i = 1 to 6
		                    digits(val(mid(candf,i,1))) = digits(val(mid(candf,i,1))) + 1
		                  next
		                  for i = 0 to 9
		                    if digits(i) = 2 then
		                      check = true
		                    end
		                  next
		                  if check then
		                    total = total + 1
		                  end
		                end
		              end
		            next
		          next
		        next
		      Next
		    next
		  next
		  
		  return str(total)
		End Function
	#tag EndMethod


#tag EndWindowCode

#tag Events runButton
	#tag Event
		Sub Action()
		  select case dayPopupMenu.text
		  case "01a"
		    'outputArea.Text = day01a(inputArea.text)
		  case "01b"
		    'outputArea.Text = day01b(inputArea.text)
		  case "02a"
		    outputArea.Text = day02a(inputArea.text)
		  case "02b"
		    outputArea.Text = day02b(inputArea.text)
		  case "03a"
		    outputArea.Text = day03a(inputArea.text)
		  case "03b"
		    outputArea.Text = day03b(inputArea.text)
		  case "04a"
		    outputArea.Text = day04a(inputArea.text)
		  case "04b"
		    outputArea.Text = day04b(inputArea.text)
		  case "05a"
		    'outputArea.Text = day05a(inputArea.text)
		  case "05b"
		    'outputArea.Text = day05b(inputArea.text)
		  case "06a"
		    'outputArea.Text = day06a(inputArea.text)
		  case "06b"
		    'outputArea.Text = day06b(inputArea.text)
		  case "07a"
		    'outputArea.Text = day07a(inputArea.text)
		  case "07b"
		    'outputArea.Text = day07b(inputArea.text)
		  case "08a"
		    'outputArea.Text = day08a(inputArea.text)
		  case "08b"
		    'outputArea.Text = day08b(inputArea.text)
		  case "09a"
		    'outputArea.Text = day09a(inputArea.text)
		  case "09b"
		    'outputArea.Text = day09b(inputArea.text)
		  case "10a"
		    'outputArea.Text = day10a(inputArea.text)
		  case "10b"
		    'outputArea.Text = day10b(inputArea.text)
		  case "11a"
		    'outputArea.Text = day11a(inputArea.text)
		  case "11b"
		    'outputArea.Text = day11b(inputArea.text)
		  case "12a"
		    'outputArea.Text = day12a(inputArea.text)
		  case "12b"
		    'outputArea.Text = day12b(inputArea.text)
		  case "13a"
		    'outputArea.Text = day13a(inputArea.text)
		  case "13b"
		    'outputArea.Text = day13b(inputArea.text)
		  case "14a"
		    'outputArea.Text = day14a(inputArea.text)
		  case "14b"
		    'outputArea.Text = day14b(inputArea.text)
		  case "15a"
		    'outputArea.Text = day15a(inputArea.text)
		  case "15b"
		    'outputArea.Text = day15b(inputArea.text)
		  end select
		  
		End Sub
	#tag EndEvent
#tag EndEvents
#tag ViewBehavior
	#tag ViewProperty
		Name="Name"
		Visible=true
		Group="ID"
		Type="String"
		EditorType="String"
	#tag EndViewProperty
	#tag ViewProperty
		Name="Interfaces"
		Visible=true
		Group="ID"
		Type="String"
		EditorType="String"
	#tag EndViewProperty
	#tag ViewProperty
		Name="Super"
		Visible=true
		Group="ID"
		Type="String"
		EditorType="String"
	#tag EndViewProperty
	#tag ViewProperty
		Name="Width"
		Visible=true
		Group="Size"
		InitialValue="600"
		Type="Integer"
	#tag EndViewProperty
	#tag ViewProperty
		Name="Height"
		Visible=true
		Group="Size"
		InitialValue="400"
		Type="Integer"
	#tag EndViewProperty
	#tag ViewProperty
		Name="MinWidth"
		Visible=true
		Group="Size"
		InitialValue="64"
		Type="Integer"
	#tag EndViewProperty
	#tag ViewProperty
		Name="MinHeight"
		Visible=true
		Group="Size"
		InitialValue="64"
		Type="Integer"
	#tag EndViewProperty
	#tag ViewProperty
		Name="MaxWidth"
		Visible=true
		Group="Size"
		InitialValue="32000"
		Type="Integer"
	#tag EndViewProperty
	#tag ViewProperty
		Name="MaxHeight"
		Visible=true
		Group="Size"
		InitialValue="32000"
		Type="Integer"
	#tag EndViewProperty
	#tag ViewProperty
		Name="Frame"
		Visible=true
		Group="Frame"
		InitialValue="0"
		Type="Integer"
		EditorType="Enum"
		#tag EnumValues
			"0 - Document"
			"1 - Movable Modal"
			"2 - Modal Dialog"
			"3 - Floating Window"
			"4 - Plain Box"
			"5 - Shadowed Box"
			"6 - Rounded Window"
			"7 - Global Floating Window"
			"8 - Sheet Window"
			"9 - Metal Window"
			"11 - Modeless Dialog"
		#tag EndEnumValues
	#tag EndViewProperty
	#tag ViewProperty
		Name="Title"
		Visible=true
		Group="Frame"
		InitialValue="Untitled"
		Type="String"
	#tag EndViewProperty
	#tag ViewProperty
		Name="CloseButton"
		Visible=true
		Group="Frame"
		InitialValue="True"
		Type="Boolean"
		EditorType="Boolean"
	#tag EndViewProperty
	#tag ViewProperty
		Name="Resizeable"
		Visible=true
		Group="Frame"
		InitialValue="True"
		Type="Boolean"
		EditorType="Boolean"
	#tag EndViewProperty
	#tag ViewProperty
		Name="MaximizeButton"
		Visible=true
		Group="Frame"
		InitialValue="True"
		Type="Boolean"
		EditorType="Boolean"
	#tag EndViewProperty
	#tag ViewProperty
		Name="MinimizeButton"
		Visible=true
		Group="Frame"
		InitialValue="True"
		Type="Boolean"
		EditorType="Boolean"
	#tag EndViewProperty
	#tag ViewProperty
		Name="FullScreenButton"
		Visible=true
		Group="Frame"
		InitialValue="False"
		Type="Boolean"
		EditorType="Boolean"
	#tag EndViewProperty
	#tag ViewProperty
		Name="Composite"
		Group="OS X (Carbon)"
		InitialValue="False"
		Type="Boolean"
	#tag EndViewProperty
	#tag ViewProperty
		Name="MacProcID"
		Group="OS X (Carbon)"
		InitialValue="0"
		Type="Integer"
	#tag EndViewProperty
	#tag ViewProperty
		Name="FullScreen"
		Group="Behavior"
		InitialValue="False"
		Type="Boolean"
		EditorType="Boolean"
	#tag EndViewProperty
	#tag ViewProperty
		Name="ImplicitInstance"
		Visible=true
		Group="Behavior"
		InitialValue="True"
		Type="Boolean"
		EditorType="Boolean"
	#tag EndViewProperty
	#tag ViewProperty
		Name="LiveResize"
		Group="Behavior"
		InitialValue="True"
		Type="Boolean"
		EditorType="Boolean"
	#tag EndViewProperty
	#tag ViewProperty
		Name="Placement"
		Visible=true
		Group="Behavior"
		InitialValue="0"
		Type="Integer"
		EditorType="Enum"
		#tag EnumValues
			"0 - Default"
			"1 - Parent Window"
			"2 - Main Screen"
			"3 - Parent Window Screen"
			"4 - Stagger"
		#tag EndEnumValues
	#tag EndViewProperty
	#tag ViewProperty
		Name="Visible"
		Visible=true
		Group="Behavior"
		InitialValue="True"
		Type="Boolean"
		EditorType="Boolean"
	#tag EndViewProperty
	#tag ViewProperty
		Name="HasBackColor"
		Visible=true
		Group="Background"
		InitialValue="False"
		Type="Boolean"
	#tag EndViewProperty
	#tag ViewProperty
		Name="BackColor"
		Visible=true
		Group="Background"
		InitialValue="&hFFFFFF"
		Type="Color"
	#tag EndViewProperty
	#tag ViewProperty
		Name="Backdrop"
		Visible=true
		Group="Background"
		Type="Picture"
		EditorType="Picture"
	#tag EndViewProperty
	#tag ViewProperty
		Name="MenuBar"
		Visible=true
		Group="Menus"
		Type="MenuBar"
		EditorType="MenuBar"
	#tag EndViewProperty
	#tag ViewProperty
		Name="MenuBarVisible"
		Visible=true
		Group="Deprecated"
		InitialValue="True"
		Type="Boolean"
		EditorType="Boolean"
	#tag EndViewProperty
#tag EndViewBehavior
