using System;
using System.Drawing;
using System.Collections;
using System.ComponentModel;
using System.Windows.Forms;
using System.Data;

using Infragistics.Win;
using Infragistics.Win.UltraWinGrid;

namespace UltraWinGrid_Keyboard_Search_CS
{
	/// <summary>
	/// Summary description for Form1.
	/// </summary>
	public class Form1 : System.Windows.Forms.Form
	{
		private Infragistics.Win.UltraWinGrid.UltraGrid ultraGrid1;
		private Infragistics.Win.Misc.UltraLabel ultraLabel1;
		/// <summary>
		/// Required designer variable.
		/// </summary>
		private System.ComponentModel.Container components = null;

		public Form1()
		{
			//
			// Required for Windows Form Designer support
			//
			InitializeComponent();

			//
			// TODO: Add any constructor code after InitializeComponent call
			//
		}

		/// <summary>
		/// Clean up any resources being used.
		/// </summary>
		protected override void Dispose( bool disposing )
		{
			if( disposing )
			{
				if (components != null) 
				{
					components.Dispose();
				}
			}
			base.Dispose( disposing );
		}

		#region Windows Form Designer generated code
		/// <summary>
		/// Required method for Designer support - do not modify
		/// the contents of this method with the code editor.
		/// </summary>
		private void InitializeComponent()
		{
			Infragistics.Win.Appearance appearance1 = new Infragistics.Win.Appearance();
			this.ultraGrid1 = new Infragistics.Win.UltraWinGrid.UltraGrid();
			this.ultraLabel1 = new Infragistics.Win.Misc.UltraLabel();
			((System.ComponentModel.ISupportInitialize)(this.ultraGrid1)).BeginInit();
			this.SuspendLayout();
			// 
			// ultraGrid1
			// 
			this.ultraGrid1.Anchor = (((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom) 
				| System.Windows.Forms.AnchorStyles.Left) 
				| System.Windows.Forms.AnchorStyles.Right);
			this.ultraGrid1.Location = new System.Drawing.Point(8, 64);
			this.ultraGrid1.Name = "ultraGrid1";
			this.ultraGrid1.Size = new System.Drawing.Size(520, 200);
			this.ultraGrid1.TabIndex = 0;
			this.ultraGrid1.Text = "ultraGrid1";
			this.ultraGrid1.InitializeLayout += new Infragistics.Win.UltraWinGrid.InitializeLayoutEventHandler(this.ultraGrid1_InitializeLayout);
			// 
			// ultraLabel1
			// 
			this.ultraLabel1.Anchor = ((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left) 
				| System.Windows.Forms.AnchorStyles.Right);
			appearance1.BackColor = System.Drawing.Color.White;
			appearance1.BackColor2 = System.Drawing.Color.FromArgb(((System.Byte)(192)), ((System.Byte)(255)), ((System.Byte)(255)));
			appearance1.BackGradientStyle = Infragistics.Win.GradientStyle.Vertical;
			this.ultraLabel1.Appearance = appearance1;
			this.ultraLabel1.Location = new System.Drawing.Point(8, 8);
			this.ultraLabel1.Name = "ultraLabel1";
			this.ultraLabel1.Size = new System.Drawing.Size(520, 56);
			this.ultraLabel1.TabIndex = 1;
			this.ultraLabel1.Text = @"Click a column header to sort the grid. Give the grid focus, but make sure the grid is not in edit mode. If a cell is in edit mode, press F2 to exit edit mode. Then type the characters of the text you want to find. The grid will select the row that matches what you type. The search is performed on the current sorted column. Click a column header to sort by a different column and then type to search that column.";
			// 
			// Form1
			// 
			this.AutoScaleBaseSize = new System.Drawing.Size(5, 13);
			this.ClientSize = new System.Drawing.Size(536, 270);
			this.Controls.AddRange(new System.Windows.Forms.Control[] {
																		  this.ultraLabel1,
																		  this.ultraGrid1});
			this.Name = "Form1";
			this.Text = "Keyboard Searching in WinGrid";
			this.Load += new System.EventHandler(this.Form1_Load);
			this.Closed += new System.EventHandler(this.Form1_Closed);
			((System.ComponentModel.ISupportInitialize)(this.ultraGrid1)).EndInit();
			this.ResumeLayout(false);

		}
		#endregion

		/// <summary>
		/// The main entry point for the application.
		/// </summary>
		[STAThread]
		static void Main() 
		{
			Application.Run(new Form1());
		}
		
		WinGridKeyboardSearchHelper searchHelper;
		private void Form1_Load(object sender, System.EventArgs e)
		{
			// Fill the grid with some dummy data. 
			this.ultraGrid1.DataSource = DummyDataCreator.GetData();

			// Create a new KeyBoardSearchHelper for this grid. 
			this.searchHelper = new WinGridKeyboardSearchHelper(this.ultraGrid1);
		}

		private void Form1_Closed(object sender, System.EventArgs e)
		{
			// Dispose of the search helper. 
			if (this.searchHelper != null)
				this.searchHelper.Dispose();
		}

		private void ultraGrid1_InitializeLayout(object sender, Infragistics.Win.UltraWinGrid.InitializeLayoutEventArgs e)
		{
			// Allow sorting in the grid. 
			e.Layout.Override.HeaderClickAction = HeaderClickAction.SortSingle;

			// Apply an ActiveCellAppearance and ActiveRowAppearance
			// to make the search results easier to see.
			if (!e.Layout.Appearances.Exists("ActiveCell"))
			{
				Infragistics.Win.Appearance activeCellAppearance = e.Layout.Appearances.Add("ActiveCell");
				activeCellAppearance.BackColor2 = Color.Yellow;
				activeCellAppearance.BackGradientStyle = GradientStyle.Vertical;                				
			}
			e.Layout.Override.ActiveCellAppearance = e.Layout.Appearances["ActiveCell"];

			if (!e.Layout.Appearances.Exists("ActiveRow"))
			{
				Infragistics.Win.Appearance activeRowAppearance = e.Layout.Appearances.Add("ActiveRow");
				activeRowAppearance.BackColor2 = Color.Goldenrod;
				activeRowAppearance.BackGradientStyle = GradientStyle.Vertical;                				
			}
			e.Layout.Override.ActiveRowAppearance = e.Layout.Appearances["ActiveRow"];
		}
	}
}
