using System;
using System.Data;
using System.Drawing;
using System.Windows.Forms;

	/// <summary>
	/// Summary description for DummyDataCreator.
	/// </summary>
	public class DummyDataCreator
	{
		#region Private Members
		private static Random rnd = new Random();
		#endregion Pricate Members

		#region Enums
		public enum Element
		{
			Air,
			Earth,
			Fire,
			Water,
			Energy,
		}
		#endregion Enums

		#region GetData

		public static DataTable GetData(int rows, Type[] types)
		{
			DataTable ReturnValue = new DataTable();
			System.Collections.Hashtable typeCounts = new System.Collections.Hashtable();
			DataColumn keyColumn = null;

			keyColumn = ReturnValue.Columns.Add("Key", typeof(int));
			keyColumn.AutoIncrement = true;
			keyColumn.ReadOnly = true;

			foreach (Type type in types)
			{
				if (typeCounts.ContainsKey(type.Name))
					typeCounts[type.Name] = (int)typeCounts[type.Name] + 1;
				else
					typeCounts.Add(type.Name, 1);

				ReturnValue.Columns.Add(type.Name + " " + typeCounts[type.Name].ToString(), type);
			}

			for (int i = 0; i < rows; i++)
			{
				DataRow newRow = ReturnValue.NewRow();
				newRow["Key"] = i;
				foreach (DataColumn dc in ReturnValue.Columns)
				{
					if (dc.DataType == typeof(string))
						newRow[dc.ColumnName] = RandomString();
					else if (dc.DataType == typeof(int)
						&& dc.ColumnName != "Key")
						newRow[dc.ColumnName] = RandomInt();
					else if (dc.DataType == typeof(bool))
						newRow[dc.ColumnName] = RandomBool();
					else if (dc.DataType == typeof(double))
						newRow[dc.ColumnName] = RandomDouble();
					else if (dc.DataType == typeof(Element))
						newRow[dc.ColumnName] = RandomElement();
					else if (dc.DataType == typeof(DateTime))
						newRow[dc.ColumnName] = RandomDate();
					else if (dc.DataType == typeof(decimal))
						newRow[dc.ColumnName] = RandomDecimal();
					else if (dc.DataType == typeof(Color))
						newRow[dc.ColumnName] = RandomColor();
					else if (dc.DataType == typeof(Image))
						newRow[dc.ColumnName] = GetTextBitmap(ReturnValue.Rows.Count.ToString());
				}
				ReturnValue.Rows.Add(newRow);
			}

			ReturnValue.AcceptChanges();
			return ReturnValue;
		}



		public static DataTable GetData(int rows)
		{
			System.Type[] types = new Type[] {typeof(string), 
												 typeof(string),
												 typeof(bool), 
												 typeof(bool), 
												 typeof(int), 
												 typeof(int), 
												 typeof(double), 
												 typeof(double), 
												 typeof(decimal), 
												 typeof(decimal), 
												 typeof(DateTime), 
												 typeof(DateTime), 
												 typeof(Element), 
												 typeof(Element),
												 typeof(Color), 
												 typeof(Color),
												 typeof(Image), 
												 typeof(Image),   
											 };
			return GetData(rows, types);
		}


		public static DataTable GetData()
		{
			return GetData(100);
		}


		#endregion GetData

		#region RandomString

		public static string RandomString(int characterCount)
		{
			if (characterCount < 1)
				characterCount = rnd.Next(1, 51);

			System.Text.StringBuilder SB = new System.Text.StringBuilder();

			for (int i = 1; i < characterCount; i++)
			{
				SB.Append(RandomCharacter());
			}

			return SB.ToString();
		}


		public static string RandomString()
		{
			return RandomString(-1);
		}
		#endregion RandomString

		#region RandomCharacter
		public static char RandomCharacter()
		{
			char ReturnValue;
			ReturnValue = (char)(rnd.Next(65, 91));

			if (RandomBool())
				ReturnValue = char.ToLower(ReturnValue);

			return ReturnValue;
		}
		#endregion RandomCharacter

		#region RandomBool
		public static bool RandomBool()
		{

			if (RandomInt(0, 1) == 1)
				return true;

			return false;
		}
		#endregion RandomBool

		#region RandomInt
		public static int RandomInt(int Min, int Max)
		{
			return rnd.Next(Min, Max + 1);
		}
		public static int RandomInt()
		{
			return RandomInt(-100, 100);
		}
		#endregion RandomInt

		#region RandomDouble
		public static Double RandomDouble()
		{
			return rnd.NextDouble() * 100;
		}
		#endregion RandomDouble

		#region RandomDecimal
		public static decimal RandomDecimal()
		{
			return (decimal)rnd.NextDouble() * 100;
		}
		#endregion RandomDecimal

		#region RandomDate
		public static DateTime RandomDate()
		{
			DateTime ReturnValue = System.DateTime.Today;
			ReturnValue = ReturnValue.AddYears(RandomInt());
			ReturnValue = ReturnValue.AddMonths(RandomInt());
			ReturnValue = ReturnValue.AddDays(RandomInt());
			ReturnValue = ReturnValue.AddHours(RandomInt());
			ReturnValue = ReturnValue.AddSeconds(RandomInt());
			ReturnValue = ReturnValue.AddMinutes(RandomInt());
			return ReturnValue;
		}
		#endregion RandomDate

		#region RandomElement
		public static Enum RandomElement()
		{
			return (Element)rnd.Next(0, System.Enum.GetValues(typeof(Element)).Length);
		}
		#endregion RandomElement

		#region RandomColor
		public static Color RandomColor()
		{
			int R, G, B;
			R = RandomInt(0, 255);
			G = RandomInt(0, 255);
			B = RandomInt(0, 255);

			Color ReturnValue = Color.FromArgb(R, G, B);
			return ReturnValue;
		}
		#endregion RandomColor

		#region  Images
		public static Bitmap GetThumnailImage(Bitmap OriginalImage, int NewWidth, int NewHeight)
		{
			return (Bitmap)OriginalImage.GetThumbnailImage(NewWidth, NewHeight, null, IntPtr.Zero);
		}

		public static Bitmap GetThumnailImage(Bitmap OriginalImage, int MaxExtent)
		{
			float AspectRatio = OriginalImage.Width / OriginalImage.Height;
			float SizeRatio;
			int NewWidth, NewHeight;

			if (AspectRatio == 0)
				return (Bitmap)OriginalImage.GetThumbnailImage(MaxExtent, MaxExtent, null, IntPtr.Zero);
			else if (AspectRatio > 1)
			{
				if (OriginalImage.Width < MaxExtent)
					return (Bitmap)OriginalImage.Clone();

				SizeRatio = MaxExtent / OriginalImage.Width;
			}
			else //AspectRation >1
			{
				if (OriginalImage.Height < MaxExtent)
					return (Bitmap)OriginalImage.Clone();

				SizeRatio = MaxExtent / OriginalImage.Height;
			}

			NewWidth = (int)((float)OriginalImage.Width * SizeRatio);
			NewHeight = (int)((float)OriginalImage.Height * SizeRatio);
			return GetThumnailImage(OriginalImage, NewWidth, NewHeight);
		}

		public enum Shape
		{
			Ellipse,
			Rectangle
		}

		public static Bitmap GetTextBitmap(string Text)
		{
			return GetTextBitmap(Text, Color.Yellow, Color.Black, Color.Black, Shape.Ellipse, -1, -1);
		}
		public static Bitmap GetTextBitmap(string Text, Color BackColor, Color ForeColor)
		{
			return GetTextBitmap(Text, BackColor, ForeColor, ForeColor, Shape.Ellipse, -1, -1);
		}
		public static Bitmap GetTextBitmap(string Text, Color BackColor, Color ForeColor, Color BorderColor)
		{
			return GetTextBitmap(Text, BackColor, ForeColor, BorderColor, Shape.Ellipse, -1, -1);
		}
		public static Bitmap GetTextBitmap(string Text, Color BackColor, Color ForeColor, Shape BorderShape)
		{
			return GetTextBitmap(Text, BackColor, ForeColor, ForeColor, BorderShape, -1, -1);
		}
		public static Bitmap GetTextBitmap(string Text, Color BackColor, Color ForeColor, Color bordercolor, Shape BorderShape)
		{
			return GetTextBitmap(Text, BackColor, ForeColor, bordercolor, BorderShape, -1, -1);
		}
		public static Bitmap GetTextBitmap(string Text, Color BackColor, Color ForeColor, Color OutlineColor, Shape BorderShape, int Height, int Width)
		{
			Bitmap BMP = new Bitmap(1, 1);
			Graphics g = Graphics.FromImage(BMP);
			if (Width == -1 || Height == -1)
			{
				SizeF CalculatedAutoSize;
				CalculatedAutoSize = g.MeasureString(Text, Control.DefaultFont);
				if (Width == -1)
					Width = (int)(CalculatedAutoSize.Width + 3);
				if (Height == -1)
					Height = (int)(CalculatedAutoSize.Height + 1);
			}

			BMP = new Bitmap(Width, Height);
			g = Graphics.FromImage(BMP);
			g.Clear(Color.Transparent);
			Rectangle R = new Rectangle(0, 0, Width - 1, Height - 1);
			System.Drawing.SolidBrush BackColorBrush = new System.Drawing.SolidBrush(BackColor);
			System.Drawing.Pen ForeColorPen = new System.Drawing.Pen(ForeColor);
			System.Drawing.Pen OutlineColorPen = new System.Drawing.Pen(OutlineColor);
			switch (BorderShape)
			{
				case Shape.Ellipse:
					g.FillEllipse(BackColorBrush, R);
					g.DrawEllipse(OutlineColorPen, R);
					break;
				case Shape.Rectangle:
					g.FillRectangle(BackColorBrush, R);
					g.DrawRectangle(OutlineColorPen, R);
					break;
			}

			StringFormat SF = new StringFormat(StringFormatFlags.NoWrap);

			SF.Alignment = StringAlignment.Center;
			SF.LineAlignment = StringAlignment.Center;
			SF.Trimming = StringTrimming.None;

			System.Drawing.SolidBrush ForeColorBrush = new System.Drawing.SolidBrush(ForeColor);

			RectangleF RF = new RectangleF(0, 0, Width, Height);

			g.DrawString(Text, Control.DefaultFont, ForeColorBrush, RF, SF);
			SF.Dispose();
			ForeColorBrush.Dispose();
			BackColorBrush.Dispose();
			ForeColorPen.Dispose();
			OutlineColorPen.Dispose();
			g.Dispose();
			return BMP;
		}
		#endregion
	}


