using System;
using System.Windows.Forms;
using Infragistics.Win.UltraWinGrid;

using System.Diagnostics;


	/// <summary>
	/// This class will work with an UltraGrid and provide keyboard searching
	/// functionality.
	/// 
	/// In order for this to work, there are 3 conditions that must be met:
	///		1) The grid must have focus.
	///		2) There must be at least one sorted column in the grid. The
	///			search takes place on the first (primary) sorted column.
	///		3) A cell cannot be in edit mode. If a cell is in edit mode, the 
	///			assumption is that the user wants to edit the cell, not 
	///			search.
	///		
	///		Also, the class only searches the root (Band 0) level of the 
	///			grid. It is intended for use with a flat list of data. 
	///			
	///		As long as these conditions are met, the user may type on the 
	///			keyboard and cell whose value matches what is typed will be 
	///			scrolled into view and the cell activated.
	///			
	///		The search behavior is similar to that of Windows Explorer. 
	///			Characters that are typed in rapid succession are assumed to 
	///			be part of a single search. If there is a delay between 
	///			characters, then a new search begins. This delay can be 
	///			adjusted using the BufferTimeOut property. The default is 
	///			500 milliseconds (half a second).
	/// </summary>
public class WinGridKeyboardSearchHelper : IDisposable
{
	#region Private Members
	
	private UltraGrid grid = null;
	private Timer timer = null;
	private int bufferTimeOut = 500;
	private string keyboardBuffer = string.Empty;	
	private bool enabled = true;

	#endregion Private Members

	#region Constructor
	/// <summary>
	/// Initializes a new instances of the WinGridKeyboardSearchHelper
	/// </summary>
	/// <param name="grid">The grid that will have keyboard searching capability.</param>
	public WinGridKeyboardSearchHelper(UltraGrid grid)
	{	
		// Set the grid property. 
		this.Grid = grid;			
	}
	#endregion Constructor

	#region Public Properties

	#region BufferTimeOut
	/// <summary>
	/// The time (in milliseconds) before the keyboard search times out.
	/// </summary>
	/// <remarks>
	/// When user types a key and the grid has focus, the timer starts. If
	/// the user types another key before the time elapsed, then the timer
	/// starts over. If the time elapses, the keyboard buffer is cleared
	/// and a new search begins.
	/// </remarks>
	public int BufferTimeOut
	{
		get
		{
			return this.bufferTimeOut;
		}
		set
		{
			this.bufferTimeOut = value;	
			
			// Set the Interval property on the timer.
			this.Timer.Interval = this.bufferTimeOut;
		}
	}
	#endregion BufferTimeOut

	#region Enabled
	/// <summary>
	/// Enables / disables the keyboard search functionality.
	/// </summary>
	/// <remarks>
	/// Setting this property to false will stop the keyboard search
	/// from being performed. 
	/// </remarks>
	public bool Enabled
	{
		get
		{
			return this.enabled;
		}
		set
		{
			this.enabled = value;
		}
	}
	#endregion Enabled

	#region Grid
	/// <summary>
	/// The grid that will have keyboard searching enabled. 
	/// </summary>
	public UltraGrid Grid
	{
		get
		{
			return this.grid;
		}
		set
		{
			// Unhook from the KeyPress event of the previous grid (if there was one).
			this.UnHookKeyPress();

			// Set the member.
			this.grid = value;

			// Stop the Timer, just in case it was going.
			this.StopTimer();

			// Clear the keyboard buffer.
			this.keyboardBuffer = string.Empty;

			// Hook the KeyPress event of the new grid (if there is one).
			this.HookKeyPress();
		}
	}
	#endregion Grid

	#endregion Public Properties

	#region Private Properties

	#region Timer
	/// <summary>
	/// Timer used to determine when the buffer time out expires.
	/// </summary>
	private Timer Timer
	{
		get
		{
			if (this.timer == null)
			{
				// If the timer is null, create a new one. Use the static
				// CreateTimer method to work around an issue in VS2003.
				this.timer = Infragistics.Win.Utilities.CreateTimer();

				// Set the Interval.
				this.timer.Interval = this.bufferTimeOut;

				// Hook the Tick event.
				this.timer.Tick += new EventHandler( OnTimerTick );
			}
			return this.timer;
		}
	}
	#endregion Timer

	#endregion Private Properties

	#region Private Methods

	#region StartTimer
	/// <summary>
	///  Starts the keyboard buffer timout timer
	/// </summary>
	private void StartTimer()
	{
		// Start the Timer.
		this.Timer.Start();
	}
	#endregion StartTimer

	#region StopTimer
	/// <summary>
	/// Stops the keyboard buffer timeout timer.
	/// </summary>
	private void StopTimer()
	{
		// Stop the timer, if it's not null.
		if (this.timer != null)
			this.timer.Stop();
	}
	#endregion StopTimer

	#region HookKeyPress
	/// <summary>
	/// Hooks into the KeyPress event of the grid. 
	/// </summary>
	private void HookKeyPress()
	{
		// If the grid is not null, hook the KeyPress event.
		if (this.grid != null)
			this.grid.KeyPress += new KeyPressEventHandler(this.OnKeyPress);
	}
	#endregion HookKeyPress

	#region UnHookKeyPress
	/// <summary>
	/// Unhooks from the KeyPress event of the grid. 
	/// </summary>
	private void UnHookKeyPress()
	{
		// If the grid is not null, unhook the KeyPress event.
		if (this.grid != null)
			this.grid.KeyPress -= new KeyPressEventHandler(this.OnKeyPress);
	}
	#endregion UnHookKeyPress

	#region GetGrid
	/// <summary>
	/// Gets a grid from a column
	/// </summary>
	/// <param name="column">An UltraGridColumn</param>
	/// <returns>Returns the grid to which the column belongs.</returns>
	private static UltraGrid GetGrid(UltraGridColumn column)
	{		
		// If the column is null, return null;
		if (column == null)
			return null;

		// If the layout is null, return null;
		if (column.Layout == null)
			return null;

		// Return the grid.
		return column.Layout.Grid as UltraGrid;
	}
	#endregion GetGrid

	#region FindRow
	
	/// <summary>
	/// Finds a row in the grid based on the current keyboard buffer text.
	/// </summary>
	/// <returns>An UltraGrid row where the first sorted columns value begins with the keyboard buffer value.</returns>
	/// <remarks>
	/// This method searches the grid specified by the Grid property. The search will be performed on the first sorted column in the grid. If there are no sorted columns, the method will return null. 
	/// The text that is searched for is a string that starts with the string in the current keyboard buffer. 
	/// </remarks>
	private UltraGridRow FindRow()
	{
		// Call FindRow with the current keyboardBuffer text.
		return this.FindRow(this.keyboardBuffer);
	}		
	#endregion FindRow

	#region PerformSearch
	/// <summary>
	/// Finds a row in the grid based on the current keyboard buffer text and activates the cell if found.
	/// </summary>
	/// <returns>An UltraGrid row where the first sorted columns value begins with the keyboard buffer value.</returns>
	/// <remarks>
	/// This method searches the grid specified by the Grid property. The search will be performed on the first sorted column in the grid. If there are no sorted columns, the method will return null. 
	/// The text that is searched for is a string that starts with the string in the current keyboard buffer. 
	/// This method is different from FindRow in that it automatically activates the row and cell if it finds a match.
	/// </remarks>
	private UltraGridRow PerformSearch()
	{
		// Call PerformSearch with the current keyboardBuffer text.
		return this.PerformSearch(this.keyboardBuffer);
	}		
	#endregion PerformSearch

	#region GetFirstSortedColumn
	/// <summary>
	/// Gets the first sorted column in the grid, if there is on.
	/// </summary>
	/// <returns>The first sorted column in the grid, or null if there are no sorted columns.</returns>
	private UltraGridColumn GetFirstSortedColumn()
	{
		// If the grid is null, return null;
		if (this.grid == null)
			return null;

		// If there are no sorted columns, return null;
		if (this.Grid.DisplayLayout.Bands[0].SortedColumns.Count == 0)
			return null;

		// return the first sorted column.
		return this.Grid.DisplayLayout.Bands[0].SortedColumns[0];
	}
	#endregion GetFirstSortedColumn

	#region DoesRowMeetSearchCriteria
	/// <summary>
	/// Determines if the row matches the search criteria based on the search string.
	/// </summary>
	/// <param name="row">The row to be evaluated.</param>
	/// <param name="column">The column being searched.</param>
	/// <param name="searchString">The string to search for. The searchString should be in all lower case.</param>
	/// <returns></returns>
	private static bool DoesRowMeetSearchCriteria(UltraGridRow row, UltraGridColumn column, string searchString)
	{
		// If this row is hidden, it should never be picked up by a 
		// keyboard search.
		if (row.HiddenResolved)
			return false;

		// Get the text of the specified column in this row. Convert
		// it to lower case so the search is no case sensitive.
		string rowText = row.GetCellText(column).ToLower();

		// If the row text starts with the searchString, return true
		return rowText.StartsWith(searchString);							
	}
	#endregion DoesRowMeetSearchCriteria

	#endregion Private Methods

	#region Public Methods

	#region FindRow
			
	/// <summary>
	/// Finds a row in the grid based on the specified string.
	/// </summary>
	/// <param name="searchString">A string to search for in the grid.</param>
	/// <returns>An UltraGrid row where the first sorted columns value begins with the specified searchString.</returns>
	/// <remarks>
	/// This method searches the grid specified by the Grid property. The search will be performed on the first sorted column in the grid. If there are no sorted columns, the method will return null. 
	/// The text that is searched for is a string that starts with the searchString passed in.
	/// </remarks>
	public UltraGridRow FindRow(string searchString)
	{
		// Call FindRow with the first sorted column in the grid. 
		UltraGridColumn firstSortedColumn = this.GetFirstSortedColumn();
		return WinGridKeyboardSearchHelper.FindRow(searchString, firstSortedColumn);
	}

	/// <summary>
	/// Finds a row in the grid based on the specified string and column.
	/// </summary>
	/// <param name="searchString">A string to search for in the grid.</param>
	/// <param name="column">The column to search.</param>
	/// <returns>An UltraGrid row where the specified column value begins with the specified searchString.</returns>
	/// <remarks>
	/// This method searches the specified column.
	/// The text that is searched for is a string that starts with the searchString passed in.
	/// </remarks>
	public static UltraGridRow FindRow(string searchString, UltraGridColumn column)
	{
		// If the searchString is empty, return null;
		if (searchString.Length == 0)
			return null;

		// If the column is null, return null. 			
		if (column == null)
			return null;

		// Get the grid from the column. 
		UltraGrid grid = WinGridKeyboardSearchHelper.GetGrid(column);

		// If the grid is null, return null;
		if (grid == null)
			return null;			

		// If the grid has no rows, return null;
		if (grid.Rows.Count == 0)
			return null;

		// Get the ActiveRow in the grid. This will be where the search starts.
		UltraGridRow startRow = grid.ActiveRow;
		
		// If there is no active row, use the first row in the grid as the starting point.
		if (startRow == null)
			startRow = grid.Rows[0];

		// Convert the search string to lower case, so the search is not 
		// case sensitive.
		searchString = searchString.ToLower();

		// Begin at the startRow and go to the last row in the grid.
		for (int i = startRow.Index; i < grid.Rows.Count; i++)
		{
			// Get the row to be evaluated.
			UltraGridRow row = grid.Rows[i];

			// Determine if this row meets the search criteria and if so, return it. 
			if (WinGridKeyboardSearchHelper.DoesRowMeetSearchCriteria(row, column, searchString))
				return row;
		}

		// If we failed to find the row, go back up to the top of the grid
		// and search down from there. First, make sure we did not already
		// start at the top.
		if (startRow.Index > 0)
		{
			// Start at the first row in the grid and go to the original
			// starting row.
			for (int i = 0; i < startRow.Index; i++)
			{
				// Get the row to be evaluated.
				UltraGridRow row = grid.Rows[i];

				// Determine if this row meets the search criteria and if so, return it. 
				if (WinGridKeyboardSearchHelper.DoesRowMeetSearchCriteria(row, column, searchString))
					return row;
			}
		}

		// If we have gotten to this point and have not found the row, 
		// return null.
		return null;
	}
	#endregion FindRow
	
	#region PerformSearch

	/// <summary>
	/// Finds a row in the grid based on the specified searchString and activates the cell if found.
	/// </summary>
	/// <param name="searchString">A string to search for in the grid.</param>
	/// <returns>An UltraGrid row where the first sorted columns value begins with the specified searchString.</returns>
	/// <remarks>
	/// This method searches the grid specified by the Grid property. The search will be performed on the first sorted column in the grid. If there are no sorted columns, the method will return null. 
	/// The text that is searched for is a string that starts with the specified searchString. 
	/// This method is different from FindRow in that it automatically activates the row and cell if it finds a match.
	/// </remarks>
	public UltraGridRow PerformSearch(string searchString)
	{
		// Call PerformSearch using the first sorted column in the grid.
		UltraGridColumn firstSortedColumn = this.GetFirstSortedColumn();
		return WinGridKeyboardSearchHelper.PerformSearch(searchString, firstSortedColumn);
	}

	/// <summary>
	/// Finds a row in the grid based on the specified searchString and column and activates the cell if found.
	/// </summary>
	/// <param name="searchString">A string to search for in the grid.</param>
	/// <param name="column">The column to search.</param>
	/// <returns>An UltraGrid row where the specified column value begins with the specified searchString.</returns>
	/// <remarks>
	/// This method searches the specified column. 
	/// The text that is searched for is a string that starts with the specified searchString. 
	/// This method is different from FindRow in that it automatically activates the row and cell if it finds a match.
	/// </remarks>
	public static UltraGridRow PerformSearch(string searchString, UltraGridColumn column)
	{
		// Find the row.
		UltraGridRow row = WinGridKeyboardSearchHelper.FindRow(searchString, column);

		// If the row was found, scroll it into view and activate the cell.
		if (row != null)
		{
			// Get the grid.
			UltraGrid grid = WinGridKeyboardSearchHelper.GetGrid(column);
			
			// Scroll the row into view on the ActiveRowScrollRegion.
			grid.ActiveRowScrollRegion.ScrollRowIntoView(row);

			// Set the ActiveRow.
			grid.ActiveRow = row;

			// Set the ActiveCell.
			grid.ActiveCell = row.Cells[column];
		}

		// Return the row that was found.
		return row;
	}
	#endregion PerformSearch

	#endregion Public Methods

	#region Event Handlers

	#region OnTimerTick
	/// <summary>
	/// Handles the tick event of the timer.
	/// </summary>
	private void OnTimerTick(object sender, EventArgs e)
	{
		// Once the timer has elapsed, stop the timer. 
		this.StopTimer();

		// Clear the keyboard buffer. 
		this.keyboardBuffer = string.Empty;
	}
	#endregion OnTimerTick

	#region OnKeyPress
	/// <summary>
	/// Handles the KeyPress event of the grid.
	/// </summary>
	private void OnKeyPress(object sender, KeyPressEventArgs e)
	{
		// If the enabled is false, do nothing.
		if (! this.enabled)
			return;

		// If there is an active cell in the grid that is in edit
		// mode, then we don't want to respond to the key press, since 
		// this would interfere with the user typing into a cell.
		UltraGridCell activeCell = this.grid.ActiveCell;
		if (activeCell!= null &&
			activeCell.IsInEditMode)			
		{
			return;				
		}

		// Start the timer. 
		this.StartTimer();

		// Append the new keystroke onto the keyboard buffer, unless
		// it's a tab key. 
		if (e.KeyChar != '\t')
			this.keyboardBuffer += e.KeyChar;

		// Perform a search.
		this.PerformSearch();
	}
	#endregion OnKeyPress

	#endregion Event Handlers

	#region Implementation of IDisposable
	/// <summary>
	/// Should be invoked when the class is disposed.
	/// </summary>
	public void Dispose()
	{
		// If the timer isn't null, clean it up.
		if (this.timer != null)
		{
			// Unhook from the Tick event
			this.timer.Tick -= new EventHandler( OnTimerTick );

			// Dispose the timer.
			this.timer.Dispose();
		}

		// Unhook from the KeyPress event of the grid.
		this.UnHookKeyPress();
	}
	#endregion
}
