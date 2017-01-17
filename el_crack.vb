''''el_crack DLL initial solution for building and debuging 
''    el_crack DLL initial solution For building And debuging 
'''' construction dll file and solution but do not working from this solution so here you just imput already tested code for compilation and dll building 
''''
''''
Imports System.IO
Imports System.Drawing
Imports System.Runtime.InteropServices.Marshal
Imports System.Runtime.InteropServices
Imports System.Drawing.Imaging

Imports Emgu.CV
Imports Emgu.Util
Imports Emgu.CV.CvEnum
Imports Emgu.CV.Structure

Namespace crack
    Public Class el_crack
        '''''to dictionary on out
        ''  Tot_a   Tot_b   Tot_c    - Hough Binary Lines Graphs quantity   |Sketch| - |Shaort| - |Long|
        '' "|Short A/Long A|"  S_a   L_a   if  (S_a) OR (L_a) > 0 then "A-CRACK"
        '' "|Short B/Long B|"  S_b   L_b   if  (S_b) OR (L_b) > 0 then "B-CRACK"
        '' "|Short C/Long C|"  S_c   L_c   if  (S_c) OR (L_c) > 0 then "C-CRACK"
        Private L_a As Integer = 0
        Private L_b As Integer = 0
        Private L_c As Integer = 0
        Private S_a As Integer = 0
        Private S_b As Integer = 0
        Private S_c As Integer = 0
        Private crack_A As Integer = 0
        Private crack_B As Integer = 0
        Private crack_C As Integer = 0

        Public Function crack_det(filename As String)

            L_a = 0
            L_b = 0
            L_c = 0
            S_a = 0
            S_b = 0
            S_c = 0
            crack_A = 0
            crack_B = 0
            crack_C = 0

            Dim dict_CRACK As New Dictionary(Of String, Double)() ''''''Output PARAMETER

            Dim Tot_a As Integer = 0
            Dim Tot_b As Integer = 0
            Dim Tot_c As Integer = 0
            ''
            Dim img As New Image(Of Bgr, Byte)(filename)
            Dim imgnormala As Image(Of Bgr, Byte) = img.Erode(5)
            ''
            ''Bilatral filtering
            Dim kernel_size As Integer = 10
            Dim sigma_color As Integer = 50
            Dim sigma_spatial As Integer = 200
            Dim imgBilat As Image(Of Bgr, Byte) = imgnormala.SmoothBilatral(kernel_size, sigma_color, sigma_spatial)
            ''
            ''segmentation
            Dim imgBILAT_TO_LAPLAS As Image(Of Bgr, Single) = imgBilat.Convert(Of Bgr, Single)
            Dim img_SOBEL As Image(Of Bgr, Single) = imgBilat.Sobel(1, 0, 3)
            Dim img_LAPLAS As Image(Of Bgr, Single) = img_SOBEL.Laplace(17)
            ''
            ''SOBEL - LAPLAS - Subtract
            Dim img_Subtract1 As Image(Of Bgr, Byte) = img_SOBEL.Convert(Of Bgr, Byte)
            Dim img_Subtract2 As Image(Of Bgr, Byte) = img_LAPLAS.Convert(Of Bgr, Byte)
            Dim img_Subtract As Image(Of Bgr, Byte) = img_Subtract1.Sub(img_Subtract2)
            ''
            ''CONTRAST
            Dim imgLAPLAS_TOCONTRAST As Image(Of Gray, Byte) = imgBilat.Convert(Of Gray, Byte)
            Dim img_CONTRAST As Image(Of Gray, Byte) = EqualizeHist(imgLAPLAS_TOCONTRAST)
            ''
            ''CONVERT_TO_OUT
            Dim imgCONVERT_TO_OUT As Image(Of Bgr, Byte) = img_CONTRAST.Convert(Of Bgr, Byte)
            ''
            ''Canny filtering                      
            Dim imgCANNY As Image(Of Gray, Byte) = img_Subtract.Canny(90, 70)
            ''
            ''Hough filtering
            Dim RhoRes As Double = 2             '5
            Dim Threshold As Double = 100          '60
            Dim MinLineWidth As Double = 60     '20 - best
            Dim linegap As Integer = 30        '20 - best
            Dim ThetaRes As Double = Math.PI / 180.0
            Dim Linez()() As LineSegment2D = imgCANNY.HoughLinesBinary(RhoRes, ThetaRes, Threshold, MinLineWidth, linegap)
            ''
            ''Line drawing
            If Linez(0).Length >= 0 Then 'Greater than or equal to
                For i As Integer = 0 To Linez(0).Length - 1
                    If Linez(0)(i).Length >= 30 And Linez(0)(i).Length < 80 Then 'Greater than and less than or equal to
                        img.Draw(Linez(0)(i), New Bgr(20, 100, 200), 1) 'New Gray(60), 1) '
                        Tot_a = Tot_a + 1
                    End If
                    If Linez(0)(i).Length >= 120 And Linez(0)(i).Length < 150 Then 'Greater than and less than or equal to
                        img.Draw(Linez(0)(i), New Bgr(255, 0, 0), 4) 'New Gray(100), 2) '
                        Tot_b = Tot_b + 1
                    End If
                    If Linez(0)(i).Length >= 150 Then
                        img.Draw(Linez(0)(i), New Bgr(0, 0, 255), 4) 'New Gray(150), 3) '
                        Tot_c = Tot_c + 1
                    End If
                Next
            End If
            ''
            Dim img_OUT As Bitmap = img.ToBitmap
            ''
            Dim colorList_A As New List(Of System.Drawing.Color)
            Dim groups_A = colorList_A.GroupBy(Function(value) value).OrderByDescending(Function(g) g.Count)
            Dim grp_A As IGrouping(Of Color, Color)

            Dim colorList_B As New List(Of System.Drawing.Color)
            Dim groups_B = colorList_B.GroupBy(Function(value) value).OrderByDescending(Function(g) g.Count)
            Dim grp_B As IGrouping(Of Color, Color)

            Dim colorList_C As New List(Of System.Drawing.Color)
            Dim groups_C = colorList_C.GroupBy(Function(value) value).OrderByDescending(Function(g) g.Count)
            Dim grp_C As IGrouping(Of Color, Color)
            ''
            For x As Integer = 100 To 300
                For y As Integer = 55 To 995
                    colorList_A.Add(img_OUT.GetPixel(x, y))
                    'img_OUT.SetPixel(x, y, Color.Green)
                Next
            Next
            For x As Integer = 440 To 650
                For y As Integer = 55 To 995
                    colorList_B.Add(img_OUT.GetPixel(x, y))
                    'img_OUT.SetPixel(x, y, Color.Green)
                Next
            Next
            For x As Integer = 790 To 990
                For y As Integer = 55 To 995
                    colorList_C.Add(img_OUT.GetPixel(x, y))
                    'img_OUT.SetPixel(x, y, Color.Green)
                Next
            Next
            'A
            For Each grp_A In groups_A
                If Convert.ToInt32(grp_A(0).R) = 0 And Convert.ToInt32(grp_A(0).G) = 0 And Convert.ToInt32(grp_A(0).B) = 255 Then
                    S_a += grp_A.Count      ' short cracks
                End If
                If Convert.ToInt32(grp_A(0).R) = 255 And Convert.ToInt32(grp_A(0).G) = 0 And Convert.ToInt32(grp_A(0).B) = 0 Then
                    L_a += grp_A.Count     'long ckracs
                End If
            Next
            'B
            For Each grp_B In groups_B
                If Convert.ToInt32(grp_B(0).R) = 0 And Convert.ToInt32(grp_B(0).G) = 0 And Convert.ToInt32(grp_B(0).B) = 255 Then
                    S_b += grp_B.Count      ' short cracks
                End If
                If Convert.ToInt32(grp_B(0).R) = 255 And Convert.ToInt32(grp_B(0).G) = 0 And Convert.ToInt32(grp_B(0).B) = 0 Then
                    L_b += grp_B.Count     'long ckracs
                End If
            Next
            'C
            For Each grp_C In groups_C
                If Convert.ToInt32(grp_C(0).R) = 0 And Convert.ToInt32(grp_C(0).G) = 0 And Convert.ToInt32(grp_C(0).B) = 255 Then
                    S_c += grp_C.Count      ' short cracks
                End If
                If Convert.ToInt32(grp_C(0).R) = 255 And Convert.ToInt32(grp_C(0).G) = 0 And Convert.ToInt32(grp_C(0).B) = 0 Then
                    L_c += grp_C.Count     'long ckracs
                End If
            Next

            '''''to dictionary on out
            ''  Tot_a   Tot_b   Tot_c    - Hough Binary Lines Graphs quantity   |Sketch| - |Shaort| - |Long|
            '' "|Short A/Long A|"  S_a   L_a   if  (S_a) OR (L_a) > 0 then "A-CRACK"
            '' "|Short B/Long B|"  S_b   L_b   if  (S_b) OR (L_b) > 0 then "B-CRACK"
            '' "|Short C/Long C|"  S_c   L_c   if  (S_c) OR (L_c) > 0 then "C-CRACK"
            If S_a > 0 Or L_a > 0 Then crack_A = 1 Else crack_A = 0
            If S_b > 0 Or L_b > 0 Then crack_B = 1 Else crack_B = 0
            If S_c > 0 Or L_c > 0 Then crack_C = 1 Else crack_C = 0

            dict_CRACK.Add("crackA", crack_A)
            dict_CRACK.Add("crackB", crack_B)
            dict_CRACK.Add("crackC", crack_C)
            'Return (dict_CRACK)

            Return (img_OUT)  ' if need return image with Hough lines
        End Function
        Private Shared Function EqualizeHist(ByVal input As Image(Of Gray, Byte)) As Image(Of Gray, Byte)
            Dim output As Image(Of Gray, Byte) = New Image(Of Gray, Byte)(input.Width, input.Height)
            CvInvoke.cvEqualizeHist(input.Ptr, output.Ptr)
            Return output
        End Function

        Public Function crack_detect_dict_out(filename As String)

            L_a = 0
            L_b = 0
            L_c = 0
            S_a = 0
            S_b = 0
            S_c = 0
            crack_A = 0
            crack_B = 0
            crack_C = 0

            Dim dict_CRACK As New Dictionary(Of String, Double)() ''''''Output PARAMETER

            Dim Tot_a As Integer = 0
            Dim Tot_b As Integer = 0
            Dim Tot_c As Integer = 0
            ''
            Dim img As New Image(Of Bgr, Byte)(filename)
            Dim imgnormala As Image(Of Bgr, Byte) = img.Erode(5)
            ''
            ''Bilatral filtering
            Dim kernel_size As Integer = 10
            Dim sigma_color As Integer = 50
            Dim sigma_spatial As Integer = 200
            Dim imgBilat As Image(Of Bgr, Byte) = imgnormala.SmoothBilatral(kernel_size, sigma_color, sigma_spatial)
            ''
            ''segmentation
            Dim imgBILAT_TO_LAPLAS As Image(Of Bgr, Single) = imgBilat.Convert(Of Bgr, Single)
            Dim img_SOBEL As Image(Of Bgr, Single) = imgBilat.Sobel(1, 0, 3)
            Dim img_LAPLAS As Image(Of Bgr, Single) = img_SOBEL.Laplace(17)
            ''
            ''SOBEL - LAPLAS - Subtract
            Dim img_Subtract1 As Image(Of Bgr, Byte) = img_SOBEL.Convert(Of Bgr, Byte)
            Dim img_Subtract2 As Image(Of Bgr, Byte) = img_LAPLAS.Convert(Of Bgr, Byte)
            Dim img_Subtract As Image(Of Bgr, Byte) = img_Subtract1.Sub(img_Subtract2)
            ''
            ''CONTRAST
            Dim imgLAPLAS_TOCONTRAST As Image(Of Gray, Byte) = imgBilat.Convert(Of Gray, Byte)
            Dim img_CONTRAST As Image(Of Gray, Byte) = EqualizeHist(imgLAPLAS_TOCONTRAST)
            ''
            ''CONVERT_TO_OUT
            Dim imgCONVERT_TO_OUT As Image(Of Bgr, Byte) = img_CONTRAST.Convert(Of Bgr, Byte)
            ''
            ''Canny filtering                      
            Dim imgCANNY As Image(Of Gray, Byte) = img_Subtract.Canny(90, 70)
            ''
            ''Hough filtering
            Dim RhoRes As Double = 2             '5
            Dim Threshold As Double = 100          '60
            Dim MinLineWidth As Double = 60     '20 - best
            Dim linegap As Integer = 30        '20 - best
            Dim ThetaRes As Double = Math.PI / 180.0
            Dim Linez()() As LineSegment2D = imgCANNY.HoughLinesBinary(RhoRes, ThetaRes, Threshold, MinLineWidth, linegap)
            ''
            ''Line drawing
            If Linez(0).Length >= 0 Then 'Greater than or equal to
                For i As Integer = 0 To Linez(0).Length - 1
                    If Linez(0)(i).Length >= 30 And Linez(0)(i).Length < 80 Then 'Greater than and less than or equal to
                        img.Draw(Linez(0)(i), New Bgr(20, 100, 200), 1) 'New Gray(60), 1) '
                        Tot_a = Tot_a + 1
                    End If
                    If Linez(0)(i).Length >= 120 And Linez(0)(i).Length < 150 Then 'Greater than and less than or equal to
                        img.Draw(Linez(0)(i), New Bgr(255, 0, 0), 4) 'New Gray(100), 2) '
                        Tot_b = Tot_b + 1
                    End If
                    If Linez(0)(i).Length >= 150 Then
                        img.Draw(Linez(0)(i), New Bgr(0, 0, 255), 4) 'New Gray(150), 3) '
                        Tot_c = Tot_c + 1
                    End If
                Next
            End If
            ''
            Dim img_OUT As Bitmap = img.ToBitmap
            ''
            Dim colorList_A As New List(Of System.Drawing.Color)
            Dim groups_A = colorList_A.GroupBy(Function(value) value).OrderByDescending(Function(g) g.Count)
            Dim grp_A As IGrouping(Of Color, Color)

            Dim colorList_B As New List(Of System.Drawing.Color)
            Dim groups_B = colorList_B.GroupBy(Function(value) value).OrderByDescending(Function(g) g.Count)
            Dim grp_B As IGrouping(Of Color, Color)

            Dim colorList_C As New List(Of System.Drawing.Color)
            Dim groups_C = colorList_C.GroupBy(Function(value) value).OrderByDescending(Function(g) g.Count)
            Dim grp_C As IGrouping(Of Color, Color)
            ''
            For x As Integer = 100 To 300
                For y As Integer = 55 To 995
                    colorList_A.Add(img_OUT.GetPixel(x, y))
                    'img_OUT.SetPixel(x, y, Color.Green)
                Next
            Next
            For x As Integer = 440 To 650
                For y As Integer = 55 To 995
                    colorList_B.Add(img_OUT.GetPixel(x, y))
                    'img_OUT.SetPixel(x, y, Color.Green)
                Next
            Next
            For x As Integer = 790 To 990
                For y As Integer = 55 To 995
                    colorList_C.Add(img_OUT.GetPixel(x, y))
                    'img_OUT.SetPixel(x, y, Color.Green)
                Next
            Next
            'A
            For Each grp_A In groups_A
                If Convert.ToInt32(grp_A(0).R) = 0 And Convert.ToInt32(grp_A(0).G) = 0 And Convert.ToInt32(grp_A(0).B) = 255 Then
                    S_a += grp_A.Count      ' short cracks
                End If
                If Convert.ToInt32(grp_A(0).R) = 255 And Convert.ToInt32(grp_A(0).G) = 0 And Convert.ToInt32(grp_A(0).B) = 0 Then
                    L_a += grp_A.Count     'long ckracs
                End If
            Next
            'B
            For Each grp_B In groups_B
                If Convert.ToInt32(grp_B(0).R) = 0 And Convert.ToInt32(grp_B(0).G) = 0 And Convert.ToInt32(grp_B(0).B) = 255 Then
                    S_b += grp_B.Count      ' short cracks
                End If
                If Convert.ToInt32(grp_B(0).R) = 255 And Convert.ToInt32(grp_B(0).G) = 0 And Convert.ToInt32(grp_B(0).B) = 0 Then
                    L_b += grp_B.Count     'long ckracs
                End If
            Next
            'C
            For Each grp_C In groups_C
                If Convert.ToInt32(grp_C(0).R) = 0 And Convert.ToInt32(grp_C(0).G) = 0 And Convert.ToInt32(grp_C(0).B) = 255 Then
                    S_c += grp_C.Count      ' short cracks
                End If
                If Convert.ToInt32(grp_C(0).R) = 255 And Convert.ToInt32(grp_C(0).G) = 0 And Convert.ToInt32(grp_C(0).B) = 0 Then
                    L_c += grp_C.Count     'long ckracs
                End If
            Next

            '''''to dictionary on out
            ''  Tot_a   Tot_b   Tot_c    - Hough Binary Lines Graphs quantity   |Sketch| - |Shaort| - |Long|
            '' "|Short A/Long A|"  S_a   L_a   if  (S_a) OR (L_a) > 0 then "A-CRACK"
            '' "|Short B/Long B|"  S_b   L_b   if  (S_b) OR (L_b) > 0 then "B-CRACK"
            '' "|Short C/Long C|"  S_c   L_c   if  (S_c) OR (L_c) > 0 then "C-CRACK"
            If S_a > 0 Or L_a > 0 Then crack_A = 1 Else crack_A = 0
            If S_b > 0 Or L_b > 0 Then crack_B = 1 Else crack_B = 0
            If S_c > 0 Or L_c > 0 Then crack_C = 1 Else crack_C = 0

            dict_CRACK.Add("crackA", crack_A)
            dict_CRACK.Add("crackB", crack_B)
            dict_CRACK.Add("crackC", crack_C)

            Return (dict_CRACK)
        End Function
    End Class

    ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
    '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''MAIN FUNCTION '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
    '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''MAIN FUNCTION '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
    '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''MAIN FUNCTION '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
    ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
    Public Class MAINCLC
        Private otp_a As Double  ' otp detect a sub cell
        Private otp_b As Double
        Private otp_c As Double

        Private otp_aG As Double  ' otp detect a sub cell
        Private otp_bG As Double
        Private otp_cG As Double

        Private otp_XX As Double ' Horizontal 
        Private otp_YY As Double ' Vertecal

        Private otp_otp As Double = Nothing
        Private otp_gli As Double = Nothing
        Private shad As Double = Nothing

        Private coord_X1 As Double
        Private coord_X2 As Double

        Private coord_Y1 As Double
        Private coord_Y2 As Double

        Private ALL_DETECTOR_OUT As Double
        Private Resolut As Integer = 52675 'Pixels per zone
        Public bm As Bitmap

        Private BAD_CO As Integer
        Private Accept_CO As Integer
        Private GOOD_CO As Integer

        Private sum_W1 As Double
        Private sum_MG1 As Double
        Private sum_BLU1 As Double
        Private sum_BLA1 As Double

        Private XXtop As Double                              ''''''INPUT PARAMETERS
        Private XXbot As Double                              ''''''INPUT PARAMETERS

        Private XXtopA As Double                              ''''''INPUT PARAMETERS
        Private XXbotA As Double                              ''''''INPUT PARAMETERS
        Private XXtopB As Double                              ''''''INPUT PARAMETERS
        Private XXbotB As Double                              ''''''INPUT PARAMETERS
        Private XXtopC As Double                              ''''''INPUT PARAMETERS
        Private XXbotC As Double                              ''''''INPUT PARAMETERS

        Public Function MAIN(filename As String)
            bm = New Bitmap(filename)                        '''''BITMAP
            Dim ax, adx1, adx2, adx3, adx4 As Integer
            Dim Rang_GRAD As Integer = 60                    '''''GRADI RANGE 
            Dim Rang_GSC_ As Integer = 30                    '''''RANGE FOR Grey Scale level
            Dim B_D_L_ As Integer = 60                       ''''Bottom DARK LEVEL 
            Dim T_DIM_L_ As Integer = 140                    ''''Top Dim Lvl
            Dim B_Light_L_ As Integer = 200                  ''''Bottom Light Lvl
            Dim U_N_F_lvl_ As Integer = 20                   ''''Uniform Level

            Dim XX As String = Nothing
            Dim dict As New Dictionary(Of String, Double)() ''''''Output PARAMETER

            For vv As Integer = 1 To 3
                Dim RES_ARRAY1 As New ArrayList              ''''''Output PARAMETER
                Select Case vv                               '''''' CASES
                    Case 1
                        coord_X1 = 70
                        coord_X2 = 314
                        coord_Y1 = 100
                        coord_Y2 = 314
                        XXtop = ALL_Detector()
                        XXtopA = ALL_Detector()

                        coord_X1 = 70
                        coord_X2 = 314
                        coord_Y1 = 750
                        coord_Y2 = 964
                        XXbot = ALL_Detector()
                        XXbotA = ALL_Detector()
                        XX = "A"
                    Case 2
                        coord_X1 = 415
                        coord_X2 = 659
                        coord_Y1 = 100
                        coord_Y2 = 314
                        XXtop = ALL_Detector()
                        XXtopB = ALL_Detector()

                        coord_X1 = 415
                        coord_X2 = 659
                        coord_Y1 = 750
                        coord_Y2 = 964
                        XXbot = ALL_Detector()
                        XXbotB = ALL_Detector()
                        XX = "B"
                    Case 3
                        coord_X1 = 760
                        coord_X2 = 1004
                        coord_Y1 = 100
                        coord_Y2 = 314
                        XXtop = ALL_Detector()
                        XXtopC = ALL_Detector()

                        coord_X1 = 760
                        coord_X2 = 1004
                        coord_Y1 = 750
                        coord_Y2 = 964
                        XXbot = ALL_Detector()
                        XXbotC = ALL_Detector()
                        XX = "C"
                End Select

                '''''A-DARK''''''''
                adx1 = 0
                adx2 = 0
                adx3 = 0
                adx4 = 0
                If XXtop <= B_D_L_ Then adx1 = 1
                If (XXtop + Rang_GSC_) <= B_D_L_ Or (XXtop - Rang_GSC_) <= B_D_L_ Then adx2 = 1
                If XXbot <= B_D_L_ Then adx3 = 1
                If (XXbot + Rang_GSC_) <= B_D_L_ Or (XXbot - Rang_GSC_) <= B_D_L_ Then adx4 = 1
                If adx1 = 1 And adx2 = 1 And adx4 = 1 And adx3 = 0 Or adx1 = 1 And adx2 = 1 And adx3 = 1 And adx4 = 1 Or adx1 = 0 And adx2 = 1 And adx3 = 1 And adx4 = 1 Then
                    RES_ARRAY1.Add(0)
                    dict.Add("Horizontal" & XX, 0)                      'DARK

                    otp_XX = 0
                    '''''A-DARK-END'
                    '''''DIM''''''''
                Else
                    adx1 = 0
                    adx2 = 0
                    adx3 = 0
                    adx4 = 0
                    If XXtop > B_D_L_ And XXtop <= T_DIM_L_ Then adx1 = 1
                    If (XXtop + Rang_GSC_) > B_D_L_ And (XXtop + Rang_GSC_) <= T_DIM_L_ Or (XXtop - Rang_GSC_) > B_D_L_ And (XXtop - Rang_GSC_) <= T_DIM_L_ Then adx2 = 1
                    If XXbot > B_D_L_ And XXbot <= T_DIM_L_ Then adx3 = 1
                    If (XXbot + Rang_GSC_) > B_D_L_ And (XXbot + Rang_GSC_) <= T_DIM_L_ Or (XXbot - Rang_GSC_) > B_D_L_ And (XXbot - Rang_GSC_) <= T_DIM_L_ Then adx4 = 1

                    If adx1 = 1 And adx2 = 1 And adx4 = 1 And adx3 = 0 Or adx1 = 1 And adx2 = 1 And adx3 = 1 And adx4 = 1 Or adx1 = 0 And adx2 = 1 And adx3 = 1 And adx4 = 1 Then
                        RES_ARRAY1.Add(1)
                        dict.Add("Horizontal" & XX, 1)                  'DIM

                        otp_XX = 1
                        '''''A-DIM-END''''
                        '''''A-Light_GREY'
                    Else
                        adx1 = 0
                        adx2 = 0
                        adx3 = 0
                        adx4 = 0
                        If XXtop > T_DIM_L_ And XXtop <= B_Light_L_ Then adx1 = 1
                        If (XXtop + Rang_GSC_) > T_DIM_L_ And (XXtop + Rang_GSC_) <= B_Light_L_ Or (XXtop - Rang_GSC_) > T_DIM_L_ And (XXtop - Rang_GSC_) <= B_Light_L_ Then adx2 = 1
                        If XXbot > T_DIM_L_ And XXbot <= B_Light_L_ Then adx3 = 1
                        If (XXbot + Rang_GSC_) > T_DIM_L_ And (XXbot + Rang_GSC_) <= B_Light_L_ Or (XXbot - Rang_GSC_) > T_DIM_L_ And (XXbot - Rang_GSC_) <= B_Light_L_ Then adx4 = 1

                        If adx1 = 1 And adx2 = 1 And adx4 = 1 And adx3 = 0 Or adx1 = 1 And adx2 = 1 And adx3 = 1 And adx4 = 1 Or adx1 = 0 And adx2 = 1 And adx3 = 1 And adx4 = 1 Then
                            RES_ARRAY1.Add(2)
                            dict.Add("Horizontal" & XX, 2)              'GREY

                            otp_XX = 2
                            '''''Light_GREY-END'
                            '''''Light''''''''''
                        Else
                            adx1 = 0
                            adx2 = 0
                            adx3 = 0
                            adx4 = 0
                            If XXtop > B_Light_L_ Then adx1 = 1
                            If XXbot > B_Light_L_ Then adx3 = 1
                            If (XXtop + Rang_GSC_) > B_Light_L_ Then adx2 = 1
                            If (XXbot + Rang_GSC_) > B_Light_L_ Then adx4 = 1
                            If adx1 = 1 And adx3 = 1 Or adx1 = 1 And adx3 = 1 And adx2 = 1 And adx4 = 1 Then
                                RES_ARRAY1.Add(3)
                                dict.Add("Horizontal" & XX, 3)          'Light
                                otp_XX = 3
                            Else
                                RES_ARRAY1.Add(4)
                                dict.Add("Horizontal" & XX, 4)          '####
                                otp_XX = 4
                            End If
                        End If
                    End If
                End If                    '''''Light-END

                '''''''''''''''Gradients''''''''''''''''
                ax = Nothing

                If (XXtop - XXbot) >= 0 And
                   (XXtop - XXbot) <= U_N_F_lvl_ Or
                   (XXbot - XXtop) >= 0 And
                   (XXbot - XXtop) <= U_N_F_lvl_ Then
                    RES_ARRAY1.Add(9)
                    dict.Add("Vertical" & XX, 9)                         '<lvl>
                    otp_YY = 9
                Else
                    If XXtop < XXbot Then ax = 1                        'DOW Small
                    If (XXtop + Rang_GRAD) < XXbot Then
                        ax = 0                                          'Dow Big
                    Else
                        If XXbot < XXtop Then ax = 3                    ' UP Small
                        If (XXbot + Rang_GRAD) < XXtop Then
                            ax = 2                                      ' UP Big
                        End If
                    End If
                    If ax = 0 Then                                      'Dow Big
                        RES_ARRAY1.Add(8)
                        dict.Add("Vertical" & XX, 8)                    'Dow Big
                        otp_YY = 8
                    End If
                    If ax = 1 Then                                      'DOW Small
                        RES_ARRAY1.Add(6)
                        dict.Add("Vertical" & XX, 6)                    'DOW Small
                        otp_YY = 6
                    End If
                    If ax = 2 Then                                      'UP Big
                        RES_ARRAY1.Add(7)
                        dict.Add("Vertical" & XX, 7)                    'UP Big
                        otp_YY = 7
                    End If
                    If ax = 3 Then                                      'UP Small
                        RES_ARRAY1.Add(5)
                        dict.Add("Vertical" & XX, 5)                    'UP Small
                        otp_YY = 5
                    End If
                End If
                Select Case vv
                    Case 1
                        otp_a = otp_XX
                        otp_aG = otp_YY
                    Case 2
                        otp_b = otp_XX
                        otp_bG = otp_YY
                    Case 3
                        otp_c = otp_XX
                        otp_cG = otp_YY
                End Select
            Next
            '''''''''''''''''''''''CONDITIONS''''''''''''''''''''''''''''
            OutofPocket_conditions()        '''''OTP'''''''''''
            dict.Add("OTP", otp_otp)        '''''OTP'''''''''''

            contact_issue_conditions()      ''''CONDITIONS'''''
            dict.Add("Contact", otp_gli)    ''''CONTACT ISSUE''

            Shadow()
            dict.Add("shadow", shad)        ''''SHADOW DEFECT''

            colordetector()                 '''''dict''''''''''
            dict.Add("CONDIT", BAD_CO)
            ''''''''''''''''''''END''''''''''''''''''''''''''''
            Return (dict)

        End Function

        '''''''picture detection function 
        Private Function ALL_Detector() '''''''picture detection function 
            Dim colorList As New List(Of System.Drawing.Color)
            For x As Integer = coord_X1 To coord_X2
                For y As Integer = coord_Y1 To coord_Y2
                    colorList.Add(bm.GetPixel(x, y))
                Next
            Next
            Dim sss As Double
            Dim res = colorList.ToArray()
            For Each grp In res
                sss += Convert.ToInt32(grp.R())
            Next
            ALL_DETECTOR_OUT = ((sss / Resolut).ToString("N3"))
            Return (ALL_DETECTOR_OUT)
        End Function

        ''''''Contact issue conditions sub  (should redo this conditions)
        Private Sub contact_issue_conditions() ''''''Contact issue conditions sub
            If otp_bG = 7 And otp_cG = 8 Or
              otp_aG = 7 And otp_bG = 8 Or
              otp_bG = 8 And otp_cG = 7 Or
              otp_aG = 8 And otp_bG = 7 Or
              otp_aG = 7 And otp_cG = 8 Or
              otp_aG = 8 And otp_cG = 7 Or
              otp_a = 2 And otp_b = 1 And otp_c = 2 Or
              otp_a = 4 And otp_b = 0 And otp_c = 4 Or
              otp_a = 1 And otp_b = 0 And otp_c = 1 Or
              otp_a = 1 And otp_b = 1 And otp_c = 2 Or
              otp_a = 2 And otp_b = 1 And otp_c = 1 Or
              otp_a = 3 And otp_b = 1 And otp_c = 2 Then

                otp_gli = 1
            Else
                otp_gli = 0
            End If
        End Sub

        '''''' OUT-OF_POCKET DETECTION CONDITION ''''
        Private Sub OutofPocket_conditions() '''''' OUT-OF_POCKET DETECTION CONDITION ''''
            If otp_a = 1 And otp_b = 1 And otp_c = 0 And otp_aG = 5 And otp_bG = 6 And otp_cG = 6 Or
                       otp_a = 1 And otp_b = 4 And otp_c = 4 And otp_aG = 5 And otp_bG = 7 And otp_cG = 7 Or
                       otp_a = 1 And otp_b = 4 And otp_c = 1 And otp_aG = 8 And otp_bG = 8 And otp_cG = 6 Or
                       otp_a = 1 And otp_b = 1 And otp_c = 4 And otp_aG = 7 And otp_bG = 5 And otp_cG = 7 Or
                       otp_a = 0 And otp_b = 0 And otp_c = 1 And otp_aG = 9 And otp_bG = 6 And otp_cG = 9 Or '5
                       otp_a = 0 And otp_b = 4 And otp_c = 2 And otp_aG = 9 And otp_bG = 8 And otp_cG = 5 Or
                       otp_a = 1 And otp_b = 1 And otp_c = 4 And otp_aG = 7 And otp_bG = 7 And otp_cG = 7 Or
                       otp_a = 1 And otp_b = 4 And otp_c = 0 And otp_aG = 8 And otp_bG = 8 And otp_cG = 6 Or
                       otp_a = 1 And otp_b = 1 And otp_c = 4 And otp_aG = 5 And otp_bG = 7 And otp_cG = 7 Or
                       otp_a = 0 And otp_b = 1 And otp_c = 2 And otp_aG = 5 And otp_bG = 7 And otp_cG = 5 Or '10
                       otp_a = 0 And otp_b = 1 And otp_c = 1 And otp_aG = 5 And otp_bG = 5 And otp_cG = 9 Or
                       otp_a = 1 And otp_b = 1 And otp_c = 0 And otp_aG = 5 And otp_bG = 7 And otp_cG = 9 Or
                       otp_a = 0 And otp_b = 1 And otp_c = 2 And otp_aG = 9 And otp_bG = 8 And otp_cG = 6 Or
                       otp_a = 1 And otp_b = 1 And otp_c = 0 And otp_aG = 7 And otp_bG = 5 And otp_cG = 9 Or
                       otp_a = 2 And otp_b = 1 And otp_c = 0 And otp_aG = 9 And otp_bG = 6 And otp_cG = 9 Or '15
                       otp_a = 2 And otp_b = 1 And otp_c = 0 And otp_aG = 6 And otp_bG = 9 And otp_cG = 9 Or
                       otp_a = 0 And otp_b = 1 And otp_c = 1 And otp_aG = 6 And otp_bG = 6 And otp_cG = 5 Or
                       otp_a = 4 And otp_b = 4 And otp_c = 1 And otp_aG = 7 And otp_bG = 7 And otp_cG = 5 Or
                       otp_a = 1 And otp_b = 4 And otp_c = 1 And otp_aG = 6 And otp_bG = 8 And otp_cG = 8 Or
                       otp_a = 4 And otp_b = 1 And otp_c = 1 And otp_aG = 7 And otp_bG = 5 And otp_cG = 7 Or '20
                       otp_a = 1 And otp_b = 0 And otp_c = 0 And otp_aG = 5 And otp_bG = 5 And otp_cG = 5 Or
                       otp_a = 2 And otp_b = 4 And otp_c = 0 And otp_aG = 5 And otp_bG = 8 And otp_cG = 9 Or
                       otp_a = 4 And otp_b = 1 And otp_c = 1 And otp_aG = 7 And otp_bG = 7 And otp_cG = 7 Or
                       otp_a = 0 And otp_b = 4 And otp_c = 1 And otp_aG = 6 And otp_bG = 8 And otp_cG = 8 Or
                       otp_a = 4 And otp_b = 1 And otp_c = 1 And otp_aG = 7 And otp_bG = 7 And otp_cG = 5 Or '25
                       otp_a = 2 And otp_b = 1 And otp_c = 0 And otp_aG = 5 And otp_bG = 7 And otp_cG = 5 Or
                       otp_a = 1 And otp_b = 1 And otp_c = 0 And otp_aG = 9 And otp_bG = 5 And otp_cG = 5 Or
                       otp_a = 0 And otp_b = 1 And otp_c = 1 And otp_aG = 9 And otp_bG = 7 And otp_cG = 5 Or
                       otp_a = 2 And otp_b = 1 And otp_c = 0 And otp_aG = 6 And otp_bG = 8 And otp_cG = 9 Or
                       otp_a = 0 And otp_b = 1 And otp_c = 1 And otp_aG = 9 And otp_bG = 5 And otp_cG = 7 Or '30
                       otp_a = 0 And otp_b = 1 And otp_c = 2 And otp_aG = 9 And otp_bG = 6 And otp_cG = 9 Or
                       otp_a = 0 And otp_b = 1 And otp_c = 2 And otp_aG = 9 And otp_bG = 9 And otp_cG = 6 Or
                       otp_a = 1 And otp_b = 4 And otp_c = 1 And otp_aG = 7 And otp_bG = 7 And otp_cG = 7 Or
                       otp_a = 0 And otp_b = 1 And otp_c = 1 And otp_aG = 9 And otp_bG = 5 And otp_cG = 5 Or
                       otp_a = 2 And otp_b = 4 And otp_c = 1 And otp_aG = 7 And otp_bG = 7 And otp_cG = 6 Or '35
                       otp_a = 1 And otp_b = 1 And otp_c = 0 And otp_aG = 6 And otp_bG = 8 And otp_cG = 6 Or
                       otp_a = 2 And otp_b = 4 And otp_c = 1 And otp_aG = 8 And otp_bG = 8 And otp_cG = 8 Or
                       otp_a = 2 And otp_b = 2 And otp_c = 1 And otp_aG = 7 And otp_bG = 7 And otp_cG = 6 Or
                       otp_a = 4 And otp_b = 4 And otp_c = 1 And otp_aG = 8 And otp_bG = 8 And otp_cG = 8 Or
                       otp_a = 4 And otp_b = 4 And otp_c = 1 And otp_aG = 7 And otp_bG = 7 And otp_cG = 7 Or '40
                       otp_a = 1 And otp_b = 1 And otp_c = 0 And otp_aG = 8 And otp_bG = 8 And otp_cG = 6 Or
                       otp_a = 0 And otp_b = 1 And otp_c = 1 And otp_aG = 5 And otp_bG = 5 And otp_cG = 5 Or
                       otp_a = 1 And otp_b = 1 And otp_c = 0 And otp_aG = 5 And otp_bG = 5 And otp_cG = 9 Or
                       otp_a = 1 And otp_b = 2 And otp_c = 2 And otp_aG = 6 And otp_bG = 7 And otp_cG = 7 Or
                       otp_a = 1 And otp_b = 4 And otp_c = 2 And otp_aG = 6 And otp_bG = 7 And otp_cG = 7 Or '45
                       otp_a = 0 And otp_b = 1 And otp_c = 1 And otp_aG = 6 And otp_bG = 8 And otp_cG = 6 Or
                       otp_a = 1 And otp_b = 4 And otp_c = 2 And otp_aG = 8 And otp_bG = 8 And otp_cG = 8 Or
                       otp_a = 1 And otp_b = 4 And otp_c = 4 And otp_aG = 8 And otp_bG = 8 And otp_cG = 8 Or
                       otp_a = 1 And otp_b = 4 And otp_c = 4 And otp_aG = 7 And otp_bG = 7 And otp_cG = 7 Or
                       otp_a = 0 And otp_b = 1 And otp_c = 1 And otp_aG = 6 And otp_bG = 8 And otp_cG = 8 Or '50 
                       otp_a = 1 And otp_b = 1 And otp_c = 0 And otp_aG = 9 And otp_bG = 8 And otp_cG = 6 Or
                       otp_a = 4 And otp_b = 1 And otp_c = 0 And otp_aG = 7 And otp_bG = 5 And otp_cG = 5 Or
                       otp_a = 4 And otp_b = 4 And otp_c = 0 And otp_aG = 8 And otp_bG = 8 And otp_cG = 6 Or
                       otp_a = 0 And otp_b = 4 And otp_c = 0 And otp_aG = 6 And otp_bG = 8 And otp_cG = 8 Or
                       otp_a = 0 And otp_b = 0 And otp_c = 0 And otp_aG = 5 And otp_bG = 5 And otp_cG = 5 Or '55
                       otp_a = 0 And otp_b = 0 And otp_c = 1 And otp_aG = 9 And otp_bG = 9 And otp_cG = 6 Or
                       otp_a = 0 And otp_b = 1 And otp_c = 1 And otp_aG = 9 And otp_bG = 6 And otp_cG = 6 Or
                       otp_a = 1 And otp_b = 0 And otp_c = 0 And otp_aG = 6 And otp_bG = 6 And otp_cG = 9 Or
                       otp_a = 1 And otp_b = 1 And otp_c = 0 And otp_aG = 5 And otp_bG = 7 And otp_cG = 5 Or
                       otp_a = 1 And otp_b = 1 And otp_c = 0 And otp_aG = 7 And otp_bG = 7 And otp_cG = 5 Or '60
                       otp_a = 1 And otp_b = 1 And otp_c = 0 And otp_aG = 6 And otp_bG = 6 And otp_cG = 9 Or
                       otp_a = 1 And otp_b = 1 And otp_c = 0 And otp_aG = 9 And otp_bG = 6 And otp_cG = 9 Or
                       otp_a = 1 And otp_b = 4 And otp_c = 1 And otp_aG = 8 And otp_bG = 8 And otp_cG = 8 Or
                       otp_a = 2 And otp_b = 1 And otp_c = 0 And otp_aG = 6 And otp_bG = 6 And otp_cG = 9 Or
                       otp_a = 2 And otp_b = 2 And otp_c = 0 And otp_aG = 8 And otp_bG = 8 And otp_cG = 6 Or '65
                       otp_a = 4 And otp_b = 1 And otp_c = 1 And otp_aG = 8 And otp_bG = 8 And otp_cG = 6 Or
                       otp_a = 4 And otp_b = 4 And otp_c = 2 And otp_aG = 8 And otp_bG = 8 And otp_cG = 6 Or
                       otp_a = 1 And otp_b = 0 And otp_c = 0 And otp_aG = 5 And otp_bG = 6 And otp_cG = 9 Then '68
                otp_otp = 1
            Else
                otp_otp = 0
            End If
        End Sub

        '''''''SHADOW conditions 
        Private Sub Shadow()        ''''''shadow conditions
            shad = Nothing
            If XXtopA <= 35 Or XXbotA <= 35 Or XXtopB <= 35 Or XXbotB <= 35 Or XXtopC <= 35 Or XXbotC <= 35 Then
                If otp_a = 4 And otp_b = 4 And otp_c = 4 And otp_aG = 8 And otp_bG = 8 And otp_cG = 8 Or
                   otp_a = 4 And otp_b = 0 And otp_c = 0 And otp_aG = 8 And otp_bG = 9 And otp_cG = 9 Or
                   otp_a = 2 And otp_b = 0 And otp_c = 0 And otp_aG = 9 And otp_bG = 9 And otp_cG = 9 Or
                   otp_a = 4 And otp_b = 1 And otp_c = 0 And otp_aG = 8 And otp_bG = 6 And otp_cG = 9 Or
                   otp_a = 4 And otp_b = 1 And otp_c = 1 And otp_aG = 7 And otp_bG = 5 And otp_cG = 9 Or '5
                   otp_a = 0 And otp_b = 0 And otp_c = 4 And otp_aG = 9 And otp_bG = 7 And otp_cG = 7 Or
                   otp_a = 4 And otp_b = 4 And otp_c = 4 And otp_aG = 7 And otp_bG = 7 And otp_cG = 7 Or
                   otp_a = 0 And otp_b = 0 And otp_c = 1 And otp_aG = 9 And otp_bG = 6 And otp_cG = 6 Or
                   otp_a = 0 And otp_b = 0 And otp_c = 2 And otp_aG = 9 And otp_bG = 9 And otp_cG = 5 Or
                   otp_a = 0 And otp_b = 0 And otp_c = 2 And otp_aG = 9 And otp_bG = 9 And otp_cG = 9 Or '10
                   otp_a = 0 And otp_b = 0 And otp_c = 3 And otp_aG = 9 And otp_bG = 9 And otp_cG = 6 Or
                   otp_a = 0 And otp_b = 1 And otp_c = 2 And otp_aG = 6 And otp_bG = 9 And otp_cG = 9 Or
                   otp_a = 0 And otp_b = 1 And otp_c = 2 And otp_aG = 9 And otp_bG = 8 And otp_cG = 9 Or
                   otp_a = 0 And otp_b = 1 And otp_c = 3 And otp_aG = 9 And otp_bG = 8 And otp_cG = 9 Or
                   otp_a = 0 And otp_b = 2 And otp_c = 3 And otp_aG = 9 And otp_bG = 9 And otp_cG = 5 Or '15
                   otp_a = 0 And otp_b = 3 And otp_c = 3 And otp_aG = 9 And otp_bG = 5 And otp_cG = 5 Or
                   otp_a = 0 And otp_b = 4 And otp_c = 2 And otp_aG = 7 And otp_bG = 7 And otp_cG = 9 Or
                   otp_a = 1 And otp_b = 1 And otp_c = 4 And otp_aG = 6 And otp_bG = 6 And otp_cG = 8 Or
                   otp_a = 1 And otp_b = 1 And otp_c = 4 And otp_aG = 9 And otp_bG = 9 And otp_cG = 7 Or
                   otp_a = 1 And otp_b = 2 And otp_c = 2 And otp_aG = 8 And otp_bG = 9 And otp_cG = 5 Or '20
                   otp_a = 2 And otp_b = 0 And otp_c = 0 And otp_aG = 6 And otp_bG = 9 And otp_cG = 9 Or
                   otp_a = 2 And otp_b = 2 And otp_c = 0 And otp_aG = 6 And otp_bG = 6 And otp_cG = 6 Or
                   otp_a = 2 And otp_b = 4 And otp_c = 0 And otp_aG = 6 And otp_bG = 8 And otp_cG = 9 Or
                   otp_a = 3 And otp_b = 0 And otp_c = 0 And otp_aG = 5 And otp_bG = 9 And otp_cG = 9 Or
                   otp_a = 3 And otp_b = 1 And otp_c = 0 And otp_aG = 9 And otp_bG = 9 And otp_cG = 9 Or '25
                   otp_a = 3 And otp_b = 4 And otp_c = 0 And otp_aG = 9 And otp_bG = 7 And otp_cG = 9 Or
                   otp_a = 3 And otp_b = 4 And otp_c = 0 And otp_aG = 6 And otp_bG = 8 And otp_cG = 6 Or
                   otp_a = 3 And otp_b = 4 And otp_c = 4 And otp_aG = 6 And otp_bG = 8 And otp_cG = 8 Or
                   otp_a = 4 And otp_b = 0 And otp_c = 0 And otp_aG = 7 And otp_bG = 9 And otp_cG = 9 Or
                   otp_a = 4 And otp_b = 0 And otp_c = 4 And otp_aG = 8 And otp_bG = 8 And otp_cG = 8 Or '30
                   otp_a = 4 And otp_b = 1 And otp_c = 0 And otp_aG = 8 And otp_bG = 9 And otp_cG = 9 Or
                   otp_a = 4 And otp_b = 2 And otp_c = 2 And otp_aG = 7 And otp_bG = 7 And otp_cG = 9 Or
                   otp_a = 4 And otp_b = 2 And otp_c = 2 And otp_aG = 7 And otp_bG = 6 And otp_cG = 9 Or
                   otp_a = 4 And otp_b = 4 And otp_c = 0 And otp_aG = 6 And otp_bG = 8 And otp_cG = 9 Or
                   otp_a = 4 And otp_b = 4 And otp_c = 0 And otp_aG = 8 And otp_bG = 8 And otp_cG = 9 Or '35
                   otp_a = 4 And otp_b = 4 And otp_c = 2 And otp_aG = 8 And otp_bG = 8 And otp_cG = 8 Or
                   otp_a = 4 And otp_b = 4 And otp_c = 2 And otp_aG = 7 And otp_bG = 7 And otp_cG = 9 Or
                   otp_a = 4 And otp_b = 4 And otp_c = 2 And otp_aG = 8 And otp_bG = 8 And otp_cG = 9 Or
                   otp_a = 4 And otp_b = 4 And otp_c = 3 And otp_aG = 8 And otp_bG = 8 And otp_cG = 9 Or
                   otp_a = 0 And otp_b = 0 And otp_c = 4 And otp_aG = 9 And otp_bG = 9 And otp_cG = 7 Or '40
                   otp_a = 4 And otp_b = 2 And otp_c = 2 And otp_aG = 7 And otp_bG = 9 And otp_cG = 9 Or
                   otp_a = 4 And otp_b = 2 And otp_c = 2 And otp_aG = 8 And otp_bG = 6 And otp_cG = 9 Or
                   otp_a = 0 And otp_b = 0 And otp_c = 0 And otp_aG = 5 And otp_bG = 7 And otp_cG = 9 Or
                   otp_a = 0 And otp_b = 0 And otp_c = 1 And otp_aG = 9 And otp_bG = 9 And otp_cG = 5 Or
                   otp_a = 0 And otp_b = 0 And otp_c = 4 And otp_aG = 9 And otp_bG = 9 And otp_cG = 8 Or '45
                   otp_a = 0 And otp_b = 3 And otp_c = 3 And otp_aG = 9 And otp_bG = 9 And otp_cG = 9 Or
                   otp_a = 0 And otp_b = 4 And otp_c = 1 And otp_aG = 9 And otp_bG = 8 And otp_cG = 9 Or
                   otp_a = 2 And otp_b = 0 And otp_c = 0 And otp_aG = 8 And otp_bG = 9 And otp_cG = 9 Or
                   otp_a = 3 And otp_b = 0 And otp_c = 0 And otp_aG = 9 And otp_bG = 9 And otp_cG = 9 Or
                   otp_a = 3 And otp_b = 3 And otp_c = 0 And otp_aG = 9 And otp_bG = 6 And otp_cG = 9 Or '50
                   otp_a = 3 And otp_b = 3 And otp_c = 0 And otp_aG = 9 And otp_bG = 9 And otp_cG = 9 Or
                   otp_a = 3 And otp_b = 4 And otp_c = 4 And otp_aG = 9 And otp_bG = 7 And otp_cG = 7 Or
                   otp_a = 4 And otp_b = 2 And otp_c = 2 And otp_aG = 8 And otp_bG = 9 And otp_cG = 9 Or
                   otp_a = 4 And otp_b = 4 And otp_c = 2 And otp_aG = 7 And otp_bG = 7 And otp_cG = 5 Or
                   otp_a = 4 And otp_b = 4 And otp_c = 2 And otp_aG = 8 And otp_bG = 6 And otp_cG = 6 Or '55
                   otp_a = 4 And otp_b = 4 And otp_c = 4 And otp_aG = 8 And otp_bG = 8 And otp_cG = 6 Or
                   otp_a = 4 And otp_b = 4 And otp_c = 2 And otp_aG = 7 And otp_bG = 6 And otp_cG = 6 Or        ''''''''''''''''''NO (corner)
                   otp_a = 4 And otp_b = 3 And otp_c = 3 And otp_aG = 8 And otp_bG = 9 And otp_cG = 9 Or        ''''''''''''''''''NO (corner)
                   otp_a = 4 And otp_b = 2 And otp_c = 2 And otp_aG = 8 And otp_bG = 6 And otp_cG = 9 Or        ''''''''''''''''''NO (corner)
                   otp_a = 1 And otp_b = 2 And otp_c = 2 And otp_aG = 8 And otp_bG = 9 And otp_cG = 5 Or '60    ''''''''''''''''''NO (corner)
                   otp_a = 2 And otp_b = 2 And otp_c = 1 And otp_aG = 6 And otp_bG = 6 And otp_cG = 7 Or        ''''''''''''''''''NO (corner)
                   otp_a = 2 And otp_b = 2 And otp_c = 4 And otp_aG = 6 And otp_bG = 6 And otp_cG = 7 Or        ''''''''''''''''''NO (corner)
                   otp_a = 2 And otp_b = 1 And otp_c = 0 And otp_aG = 6 And otp_bG = 9 And otp_cG = 9 Or        'OTP+SHD
                   otp_a = 1 And otp_b = 0 And otp_c = 0 And otp_aG = 5 And otp_bG = 6 And otp_cG = 9 Or        'OTP+SHD
                   otp_a = 1 And otp_b = 4 And otp_c = 4 And otp_aG = 5 And otp_bG = 7 And otp_cG = 7 Or '65    'OTP+SHD
                   otp_a = 0 And otp_b = 1 And otp_c = 1 And otp_aG = 9 And otp_bG = 5 And otp_cG = 7 Or
                   otp_a = 4 And otp_b = 3 And otp_c = 3 And otp_aG = 8 And otp_bG = 9 And otp_cG = 6 Or
                   otp_a = 3 And otp_b = 3 And otp_c = 0 And otp_aG = 6 And otp_bG = 9 And otp_cG = 9 Or
                   otp_a = 2 And otp_b = 3 And otp_c = 0 And otp_aG = 9 And otp_bG = 9 And otp_cG = 9 Or
                   otp_a = 2 And otp_b = 3 And otp_c = 0 And otp_aG = 6 And otp_bG = 9 And otp_cG = 9 Then '70   'OTP+SHD 
                    shad = 1
                Else
                    shad = 0
                End If
            End If
        End Sub

        '''''' BAG Conditions 
        Private Sub BAG_conditions()

            Dim White As Integer = Nothing
            Dim Magenta As Integer = Nothing
            Dim Blue As Integer = Nothing
            Dim Black As Integer = Nothing

            '''''''''CONDITIONS White
            If sum_W1 <= 1 Then                                     '    White - Trash
                White = 1
            Else
                If sum_W1 > 1 And sum_W1 <= 25 Then                 '    White - Very Bad
                    White = 2
                Else
                    If sum_W1 > 25 And sum_W1 <= 50 Then            '    White - Bad
                        White = 3
                    Else
                        If sum_W1 > 50 And sum_W1 <= 80 Then        '    White - Acceptable
                            White = 4
                        Else
                            If sum_W1 > 80 And sum_W1 <= 90 Then    '    White - Good
                                White = 5
                            Else
                                If sum_W1 > 90 Then                 '    White - superb
                                    White = 6
                                End If
                            End If
                        End If
                    End If
                End If
            End If                                                  '''''''''CONDITIONS White END

            '''''''''CONDITIONS Magenta
            If sum_MG1 <= 5 Then                                     ' Magenta - superb
                Magenta = 1
            Else
                If sum_MG1 > 5 And sum_MG1 <= 10 Then                ' Magenta - Good
                    Magenta = 2
                Else
                    If sum_MG1 > 10 And sum_MG1 <= 30 Then           ' Magenta - Acceptable
                        Magenta = 3
                    Else
                        If sum_MG1 > 30 And sum_MG1 <= 50 Then       ' Magenta - Bad
                            Magenta = 4
                        Else
                            If sum_MG1 > 50 Then                     ' Magenta - Very Bad
                                Magenta = 5
                            End If
                        End If
                    End If
                End If
            End If                                                   '''''''''CONDITIONS Magenta END

            '''''''''CONDITIONS BLUE
            If sum_BLU1 <= 1 Then                                     ' BLUE - Good 
                Blue = 1
            Else
                If sum_BLU1 > 1 And sum_BLU1 <= 10 Then               ' BLUE - Acceptable
                    Blue = 2
                Else
                    If sum_BLU1 > 10 And sum_BLU1 <= 30 Then          ' BLUE - Bad
                        Blue = 3
                    Else
                        If sum_BLU1 > 30 Then                         ' BLUE - Very Bad
                            Blue = 4
                        End If
                    End If
                End If
            End If                                                    '''''''''CONDITIONS BLUE END

            '''''''''CONDITIONS BLACK
            If sum_BLA1 <= 1 Then                                                   ' BLACK - superb
                Black = 1
            Else
                If sum_BLA1 > 1 And sum_BLA1 <= 2 Then                              ' BLACK - Good
                    Black = 2
                Else
                    If sum_BLA1 > 2 And sum_BLA1 <= 5 Then                          ' BLACK - Acceptable
                        Black = 3
                    Else
                        If sum_BLA1 > 5 And sum_BLA1 <= 7 Then                       ' BLACK - Bad
                            Black = 4
                        Else
                            If sum_BLA1 > 7 And sum_BLA1 <= 20 Then                  ' BLACK - Very bad
                                Black = 5
                            Else
                                If sum_BLA1 > 20 And sum_BLA1 <= 29 Then             ' BLACK - Trash
                                    Black = 6
                                Else
                                    If sum_BLA1 > 29 And sum_BLA1 <= 35 Then          ' BLACK - Trash - 1
                                        Black = 7
                                    Else
                                        If sum_BLA1 > 35 And sum_BLA1 <= 50 Then      ' BLACK - Trash - 2
                                            Black = 8
                                        Else
                                            If sum_BLA1 > 50 Then                     ' BLACK - Completely junk
                                                Black = 9
                                            End If
                                        End If
                                    End If
                                End If
                            End If
                        End If
                    End If
                End If
            End If                                                                   '''''''''CONDITIONS BLACK END
            '''''''''Conditions for GOOD - ACCEPTABLE - BAD
            If (Black = 4 Or Black = 5 Or Black = 6 Or Black = 7 Or Black = 8 Or Black = 9) Then
                BAD_CO = 0                                                           ' This cell is BAD

            ElseIf White = 5 Or White = 6 Then
                BAD_CO = 2                                                           ' This cell is GOOD
            Else
                BAD_CO = 1                                                           ' This cell is ACCEPTABLE
            End If
        End Sub

        ''''''' COLORPROSSESOR
        Private Sub colordetector()

            '''''' CONTRAST and filtering
            Cont(bm, 100)
            '''''' CONTRAST and filtering

            Dim sum_W As Integer
            Dim sum_MG As Integer
            Dim sum_BLU As Integer
            Dim sum_BLA As Integer

            Dim ResolutBig As Integer = 833658
            Dim colorList As New List(Of System.Drawing.Color)
            Dim groups = colorList.GroupBy(Function(value) value).OrderByDescending(Function(g) g.Count)

            For x As Integer = 45 To 335
                For y As Integer = 35 To 1000
                    colorList.Add(bm.GetPixel(x, y))
                Next
            Next
            For x As Integer = 385 To 685
                For y As Integer = 35 To 1000
                    colorList.Add(bm.GetPixel(x, y))
                Next
            Next
            For x As Integer = 745 To 1015
                For y As Integer = 35 To 1000
                    colorList.Add(bm.GetPixel(x, y))
                Next
            Next
            For Each grp In groups
                If Convert.ToInt32(grp(0).R) = 255 And Convert.ToInt32(grp(0).G) = 255 And Convert.ToInt32(grp(0).B) = 255 Then
                    sum_W += grp.Count
                End If
                If Convert.ToInt32(grp(0).R) = 255 And Convert.ToInt32(grp(0).G) = 0 And Convert.ToInt32(grp(0).B) = 255 Then
                    sum_MG += grp.Count
                End If
                If Convert.ToInt32(grp(0).R) = 0 And Convert.ToInt32(grp(0).G) = 0 And Convert.ToInt32(grp(0).B) = 255 Then
                    sum_BLU += grp.Count
                End If
                If Convert.ToInt32(grp(0).R) = 0 And Convert.ToInt32(grp(0).G) = 0 And Convert.ToInt32(grp(0).B) = 0 Then
                    sum_BLA += grp.Count
                End If

                sum_W1 = (sum_W * 100) / ResolutBig
                sum_MG1 = (sum_MG * 100) / ResolutBig
                sum_BLU1 = (sum_BLU * 100) / ResolutBig
                sum_BLA1 = (sum_BLA * 100) / ResolutBig
            Next

            '''''' BAG Conditions 
            BAG_conditions()
            '''''' BAG Conditions END
        End Sub

        ''''''' COLORPROSSESOR3
        Private Sub Cont(ByRef bm As Bitmap, contrastValue As SByte)
            If contrastValue < -100 OrElse contrastValue > 100 Then
                Return
            End If

            Dim bmpData As BitmapData = bm.LockBits(New Rectangle(0, 0, bm.Width, bm.Height), ImageLockMode.ReadWrite, PixelFormat.Format24bppRgb)
            Dim ptr As IntPtr = bmpData.Scan0
            Dim stopAddress As Integer = CInt(ptr) + bmpData.Stride * bmpData.Height
            Dim pixel As Double = 0, contrast As Double = (100 + contrastValue) / 100

            contrast *= contrast

            While CInt(ptr) <> stopAddress
                pixel = Marshal.ReadByte(ptr) / 50
                pixel -= 1
                pixel *= contrast
                pixel += 1
                pixel *= 255
                If pixel < 40 Then
                    pixel = 0
                ElseIf pixel > 41 Then
                    pixel = 255
                End If
                Marshal.WriteByte(ptr, CByte(Math.Truncate(pixel)))

                pixel = Marshal.ReadByte(ptr + 1) / 150
                pixel -= 1      ' =1 (white)   '  =25 (mag) (lighteses areas)
                pixel *= contrast
                pixel += 1      ' =1 (white)    '  =8 (mag)
                pixel *= 200
                If pixel < 10 Then
                    pixel = 0
                ElseIf pixel > 11 Then
                    pixel = 255
                End If
                Marshal.WriteByte(ptr + 1, CByte(Math.Truncate(pixel)))

                pixel = Marshal.ReadByte(ptr + 2) / 255
                pixel -= 0.3
                pixel *= contrast
                pixel += 0.3
                pixel *= 255
                If pixel < 10 Then
                    pixel = 0
                ElseIf pixel > 11 Then
                    pixel = 255
                End If
                Marshal.WriteByte(ptr + 2, CByte(Math.Truncate(pixel)))
                ptr += 3
            End While
            bm.UnlockBits(bmpData)
        End Sub
    End Class
End Namespace

